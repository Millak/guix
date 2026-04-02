;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

;; This manifest lists all MPI-related packages: useful for 'guix refresh',
;; 'guix build', etc.

(use-modules (guix)
             (gnu packages fabric-management)
             (gnu packages linux)
             (gnu packages mpi)
             (gnu packages parallel))

;; The packages below are what constitutes the MPI stack: helper libraries,
;; interconnect drivers, and MPI implementations.
(packages->manifest
 (list cassini-headers
       cxi-driver
       hwloc
       libcxi
       libfabric
       mpich
       openmpi-5
       openpmix
       opensm
       prrte
       psm
       psm2
       rdma-core
       ucx))
