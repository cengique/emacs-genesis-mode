emacs-genesis-mode
==================

Emacs major mode for editing GENESIS neuron simulator script files.

 genesis-mode.el -- Major mode for editing GENESIS neuron simulator script files.
 Modified from: wpdl-mode-el and from DerivedMode example in Emacs Wiki.

 Authors: Cengiz Gunay <cengique@users.sf.net> and 
 	    Hugo Cornelis <hugo.cornelis@gmail.com>
 WPDL-Mode Author: Scott Andrew Borton <scott@pp.htv.fi>
 Created: 30 May 2014
 Keywords: Genesis major-mode

 Copyright (C) 2014 Cengiz Gunay <cengique@users.sf.net>
 Copyright (C) 2005-2006 Hugo Cornelis
 Copyright (C) 2000, 2003 Scott Andrew Borton <scott@pp.htv.fi>

 Installation:
--------------

 Load this file in (X)Emacs. Placing it under ~/.emacs.d/ should
 make it autoload it Emacs. Otherwise place the following lines in
 your ~/.emacs file:
 	(add-to-list 'load-path "~/.emacs.d/")
 	(require 'genesis-mode)
 In XEmacs, you could place the following in your ~/.xemacs/init.el file:
 	(load-file "~/.xemacs/genesis-mode.el")

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation; either version 2 of
 the License, or (at your option) any later version.

 This program is distributed in the hope that it will be
 useful, but WITHOUT ANY WARRANTY; without even the implied
 warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 PURPOSE.  See the GNU General Public License for more details.

 You should have received a copy of the GNU General Public
 License along with this program; if not, write to the Free
 Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 MA 02111-1307 USA

Notes:
------

This mode provides:

* syntax highlighting for Genesis keywords, functions, and objects. 
* automatic indentation with the tab key
* function index menu item

This mode uses an example used in a tutorial about Emacs
mode creation. The tutorial can be found here:
http://renormalist.net/Renormalist/EmacsLanguageModeCreationTutorial

 TODO:
 - test in Xemacs


