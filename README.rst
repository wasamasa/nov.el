nov.el
======

.. image:: https://raw.github.com/wasamasa/nov.el/master/img/novels.gif

About
-----

``nov.el`` provides a major mode for reading EPUB documents.

Features:

- Basic navigation (jump to TOC, previous/next chapter)
- Jump to next chapter when scrolling beyond end
- Renders EPUB2 (.ncx) and EPUB3 (<nav>) TOCs
- Hyperlinks to internal and external targets
- Supports textual and image documents
- View source of document files
- Metadata display
- Image rescaling

Screenshot
----------

.. image:: https://raw.github.com/wasamasa/nov.el/master/img/scrot.png

Installation
------------

Set up the `MELPA <https://melpa.org/>`_ or `MELPA Stable
<https://stable.melpa.org/>`_ repository if you haven't already and
install with ``M-x package-install RET nov RET``.

Setup
-----

Make sure you have an ``unzip`` executable on ``PATH``, otherwise the
extraction of EPUB files will fail.  If you for some reason have
``unzip`` in a non-standard location, customize ``nov-unzip-program``
to its path.  You'll also need an Emacs compiled with ``libxml2``
support, otherwise rendering will fail.

Put the following in your init file:

.. code:: elisp

    (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

Customization
-------------

While the defaults make for an acceptable reading experience, it can
be improved with any of the following changes:

Default font
............

To change the default font, use ``M-x customize-face RET
variable-pitch``, pick a different family, save and apply.  If you
dislike globally customizing that face, add the following to your init
file:

.. code:: elisp

    (defun my-nov-font-setup ()
      (face-remap-add-relative 'variable-pitch :family "Liberation Serif"))
    (add-hook 'nov-mode-hook 'my-nov-font-setup)

To completely disable the variable pitch font, customize
``nov-variable-pitch`` to ``nil``.  Text will be displayed with the
default face instead which should be using a monospace font.

Usage
-----

Open the EPUB file with ``C-x C-f ~/novels/novel.epub``, scroll with
``SPC`` and switch chapters with ``n`` and ``p``.  More keybinds can
be looked up with ``F1 m``.

Contributing
------------

See `CONTRIBUTING.rst
<https://github.com/wasamasa/nov.el/blob/master/CONTRIBUTING.rst>`_.

Alternatives
------------

The first one I've heard of is `epubmode.el
<https://www.emacswiki.org/emacs/epubmode.el>`_ which is, well, see
for yourself.  You might find `ereader
<https://github.com/bddean/emacs-ereader>`_ more useful, especially if
you're after Org integration and annotation support.
