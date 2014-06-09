emacs-genesis-mode
==================

Emacs major mode for editing GENESIS neuron simulator script files.

Modified from: wpdl-mode-el and from DerivedMode example in Emacs Wiki.

Installation:
--------------

 Load `genesis-mode.el` in (X)Emacs. In *Emacs*, placing it under `~/.emacs.d/` should suffice.
 If it is not autoloaded when Emacs starts, place the following lines in
 your `~/.emacs` file:

```lisp
(add-to-list 'load-path "~/.emacs.d/")
(require 'genesis-mode)
```

 In *XEmacs*, place `genesis-mode.el` under `~/.xemacs/` and add the following in your `~/.xemacs/init.el` file:

```lisp
(load-file "~/.xemacs/genesis-mode.el")
```

Functionality:
------

This mode provides:

* syntax highlighting for Genesis keywords, functions, and objects. 
* automatic indentation with the tab key
* function index menu item

It is based on original Xemacs major mode by HC and an example used in
a tutorial about Emacs mode creation. The tutorial can be found here:
http://renormalist.net/Renormalist/EmacsLanguageModeCreationTutorial

TODO
----------

* Fix `function-menu` in Xemacs

* Unify syntax highlighting for both Emacses
* Indentation: 'end' on line after continuation ends are ignored
* Indentation: Sometimes takes previous line's wrong indentation even if it's only whitespace


CREDITS AND LICENSE
-------------

*Authors:* Cengiz Gunay (cengique AT users.sf.net) and 
 	    Hugo Cornelis (hugo.cornelis AT gmail.com)

*WPDL-Mode Author:* Scott Andrew Borton (scott AT pp.htv.fi)

*Contributors:*

* Jim Perlewitz (perlewitz AT earthlink.net) - Supplied full list of
	Genesis 2.3 functions and objects (2014/06/06).
* Zbigniew Jędrzejewski-Szmek (zjedrzej AT gmu.edu) - Bugfixes and
	testing (2014/06/05).

*Created:* 30 May 2014

*Keywords:* Genesis major-mode

Copyright (C) 2014 Cengiz Gunay 

Copyright (C) 2005-2006 Hugo Cornelis

Copyright (C) 2000, 2003 Scott Andrew Borton

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

