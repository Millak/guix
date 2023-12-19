<?php
/**
 * The content of this file is copied from composer's src/Composer/Autoload/ClassMapGenerator.php
 * the findClasses method was extracted, to prevent using any dependency.
 *
 * Composer (and thus this file) is distributed under the expat license, and
 * ClassMapGenerator.php also contains this notice:
 *
 *   This file is part of Composer.
 *
 *   (c) Nils Adermann <naderman@naderman.de>
 *       Jordi Boggiano <j.boggiano@seld.be>
 *
 *   For the full copyright and license information, please view the LICENSE
 *   file that was distributed with this source code.
 *
 *   This file is copied from the Symfony package.
 *
 *   (c) Fabien Potencier <fabien@symfony.com>
 * 
 * To the extent to wich it makes sense, as the author of the extract:
 * Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
 */

/**
 * Extract the classes in the given file
 *
 * @param  string            $path The file to check
 * @throws \RuntimeException
 * @return array             The found classes
 */
function findClasses($path)
{
    $extraTypes = PHP_VERSION_ID < 50400 ? '' : '|trait';
    if (defined('HHVM_VERSION') && version_compare(HHVM_VERSION, '3.3', '>=')) {
        $extraTypes .= '|enum';
    }
    // Use @ here instead of Silencer to actively suppress 'unhelpful' output
    // @link https://github.com/composer/composer/pull/4886
    $contents = @php_strip_whitespace($path);
    if (!$contents) {
        if (!file_exists($path)) {
            $message = 'File at "%s" does not exist, check your classmap definitions';
        } elseif (!is_readable($path)) {
            $message = 'File at "%s" is not readable, check its permissions';
        } elseif ('' === trim(file_get_contents($path))) {
            // The input file was really empty and thus contains no classes
            return array();
        } else {
            $message = 'File at "%s" could not be parsed as PHP, it may be binary or corrupted';
        }
        $error = error_get_last();
        if (isset($error['message'])) {
            $message .= PHP_EOL . 'The following message may be helpful:' . PHP_EOL . $error['message'];
        }
        throw new \RuntimeException(sprintf($message, $path));
    }
    // return early if there is no chance of matching anything in this file
    if (!preg_match('{\b(?:class|interface'.$extraTypes.')\s}i', $contents)) {
        return array();
    }
    // strip heredocs/nowdocs
    $contents = preg_replace('{<<<[ \t]*([\'"]?)(\w+)\\1(?:\r\n|\n|\r)(?:.*?)(?:\r\n|\n|\r)(?:\s*)\\2(?=\s+|[;,.)])}s', 'null', $contents);
    // strip strings
    $contents = preg_replace('{"[^"\\\\]*+(\\\\.[^"\\\\]*+)*+"|\'[^\'\\\\]*+(\\\\.[^\'\\\\]*+)*+\'}s', 'null', $contents);
    // strip leading non-php code if needed
    if (substr($contents, 0, 2) !== '<?') {
        $contents = preg_replace('{^.+?<\?}s', '<?', $contents, 1, $replacements);
        if ($replacements === 0) {
            return array();
        }
    }
    // strip non-php blocks in the file
    $contents = preg_replace('{\?>(?:[^<]++|<(?!\?))*+<\?}s', '?><?', $contents);
    // strip trailing non-php code if needed
    $pos = strrpos($contents, '?>');
    if (false !== $pos && false === strpos(substr($contents, $pos), '<?')) {
        $contents = substr($contents, 0, $pos);
    }
    // strip comments if short open tags are in the file
    if (preg_match('{(<\?)(?!(php|hh))}i', $contents)) {
        $contents = preg_replace('{//.* | /\*(?:[^*]++|\*(?!/))*\*/}x', '', $contents);
    }
    preg_match_all('{
        (?:
             \b(?<![\$:>])(?P<type>class|interface'.$extraTypes.') \s++ (?P<name>[a-zA-Z_\x7f-\xff:][a-zA-Z0-9_\x7f-\xff:\-]*+)
           | \b(?<![\$:>])(?P<ns>namespace) (?P<nsname>\s++[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*+(?:\s*+\\\\\s*+[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*+)*+)? \s*+ [\{;]
        )
    }ix', $contents, $matches);
    $classes = array();
    $namespace = '';
    for ($i = 0, $len = count($matches['type']); $i < $len; $i++) {
        if (!empty($matches['ns'][$i])) {
            $namespace = str_replace(array(' ', "\t", "\r", "\n"), '', $matches['nsname'][$i]) . '\\';
        } else {
            $name = $matches['name'][$i];
            // skip anon classes extending/implementing
            if ($name === 'extends' || $name === 'implements') {
                continue;
            }
            if ($name[0] === ':') {
                // This is an XHP class, https://github.com/facebook/xhp
                $name = 'xhp'.substr(str_replace(array('-', ':'), array('_', '__'), $name), 1);
            } elseif ($matches['type'][$i] === 'enum') {
                // In Hack, something like:
                //   enum Foo: int { HERP = '123'; }
                // The regex above captures the colon, which isn't part of
                // the class name.
                $name = rtrim($name, ':');
            }
            $classes[] = ltrim($namespace . $name, '\\');
        }
    }
    return $classes;
}

$options = getopt('i:f:', []);
$file = $options["f"];
$input = $options["i"];

$classes = findClasses($file);
foreach($classes as $class) {
  echo '$classmap[\''.$class.'\'] = \''.$input.'/'.$file.'\';';
  echo "\n";
}
