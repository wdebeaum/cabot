# CABOT git mirror #

CABOT stands for Conversing About Blocks On a Table. It is a collaborative assistant for the blocks world developed by IHMC together with University of Rochester and SIFT. The project is funded by DARPA under the Communication with Computers (CwC) program.

This git repo is a mirror of the TRIPS `cabot` CVS module. It is updated nightly.

[Installation instructions](http://trips.ihmc.us/trac/cabot/wiki/CABoTSystemInstallation).

The repo contains several libraries with their own FOSS licenses:

 * The `src/config/lisp/defsystem/defsystem-3.6i/` directory contains a modified, non-standard, non-official version of [MK:DEFSYSTEM](http://www.cliki.net/mk-defsystem) 3.6i. See the comments near the top of `defsystem.lisp` in that directory for its copyright notice and license.
 * The `src/portaudio/` directory contains a modified [portaudio](http://www.portaudio.com/) v19-r1647, which uses the MIT/Expat license. See `src/portaudio/LICENSE.txt`.
 * The `src/Sphinx3/` directory contains a modified [Sphinx](http://cmusphinx.sourceforge.net/)3, which uses the 2-clause BSD license. See `src/Sphinx3/LICENSE`. Some files in `src/SpeechIn3/` are also from Sphinx3 and have that license at the top of the file.
 * `src/GroundingModule/commons-math3-3.2edit.jar` is from [Apache Commons Math](https://commons.apache.org/proper/commons-math/), which uses the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0) (see `licenses/apache-software-license.txt`). It has likely been changed from the original.
 * `src/SRIWrapper/json-simple-1.1.1.jar` is from [json-simple](https://github.com/fangyidong/json-simple), which also uses the Apache License 2.0.
 * `src/GroundingModule/jblas-1.2.0.jar`, `src/GroundingModule/jblas-1.2.3.jar`, and `src/SRIWrapper/jblas-1.2.3.jar` are from [jblas](http://jblas.org), which uses a [3-clause BSD license](https://github.com/mikiobraun/jblas/blob/e1de8249b28137fa94a79558ee90ff037fd7c47d/COPYING) (see `licenses/jblas-license.txt`).
 * The `src/GroundingModule/qhull/` directory contains [qhull](http://www.qhull.org/) 2012.1. Its license is `src/GroundingModule/qhull/COPYING.txt`.
 * `src/SRIWrapper/uber-example-embedded-5.2-SNAPSHOT.jar` is from [MaryTTS](http://mary.dfki.de/index.html), which uses [LGPL 3](http://www.gnu.org/licenses/lgpl.html) (`licenses/lgpl-3.0.txt`), but includes dependencies using [a variety of other licenses](https://github.com/marytts/marytts/blob/master/LICENSE.md), [here too](http://mary.dfki.de/voice-cmu-slt-hsmm/dependency-management.html). These licenses are also included in the `licenses/` directory.

The rest of the repo uses [GPL 2+](http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html) (see `licenses/gpl-2.0.txt`, and the notice below). Some of the licenses listed above are only compatible with [GPL 3](http://www.gnu.org/licenses/gpl.html) (`licenses/gpl-3.0.txt`), so any TRIPS code that links to the corresponding libraries is effectively under GPL 3. But TRIPS is actually many separate programs which communicate over sockets, so not much of it is affected in this way.

TRIPS CABOT system  
Copyright (C) 2016  Institute for Human & Machine Cognition

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
