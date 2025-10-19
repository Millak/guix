#pragma once

#include <functional>
#include <string>
#include <cstdint>
#include <string_view>

#include "types.hh"

class sqlite3;
class sqlite3_stmt;

namespace nix {

/* RAII wrapper to close a SQLite database automatically. */
struct SQLite
{
    sqlite3 * db;
    SQLite() { db = 0; }
    ~SQLite();
    operator sqlite3 * () { return db; }
};

/* RAII wrapper to create and destroy SQLite prepared statements. */
struct SQLiteStmt
{
    sqlite3 * db = 0;
    sqlite3_stmt * stmt = 0;
    SQLiteStmt() { }
    void create(sqlite3 * db, const std::string & s);
    ~SQLiteStmt();
    operator sqlite3_stmt * () { return stmt; }

    /* Helper for binding / executing statements. */
    class Use
    {
        friend struct SQLiteStmt;
    private:
        SQLiteStmt & stmt;
        unsigned int curArg = 1;
        Use(SQLiteStmt & stmt);

    public:

        ~Use();

        /* Bind the next parameter. */
        Use & operator () (const std::string & value, bool notNull = true);
        Use & operator () (int64_t value, bool notNull = true);
        Use & bind(); // null

        int step();

        /* Execute a statement that does not return rows. */
        void exec();

        /* For statements that return 0 or more rows. Returns true iff
           a row is available. */
        bool next();

        std::string getStr(int col);
        int64_t getInt(int col);
    };

    Use use()
    {
        return Use(*this);
    }
};

/* RAII helper that ensures transactions are aborted unless explicitly
   committed. */
struct SQLiteTxn
{
    bool active = false;
    sqlite3 * db;

    SQLiteTxn(sqlite3 * db);

    void commit();

    ~SQLiteTxn();
};


MakeError(SQLiteError, Error);
MakeError(SQLiteBusy, SQLiteError);

[[noreturn]] void throwSQLiteError(sqlite3 * db, std::string_view f);

/* Convenience function for retrying a SQLite transaction when the
   database is busy. */
template<typename T>
T retrySQLite(std::function<T()> fun)
{
    while (true) {
        try {
            return fun();
        } catch (SQLiteBusy & e) {
        }
    }
}

}
