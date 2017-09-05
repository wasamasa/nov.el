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

You'll have to manually install `esxml
<https://github.com/tali713/esxml>`_ for the ``esxml-query.el`` file,
then this package.  Sorry about that!

Setup
-----

Make sure you have an ``unzip`` executable on ``PATH``, otherwise the
extraction of EPUB files will fail.  If you for some reason have
``unzip`` in a non-standard location, customize ``nov-unzip-program``
to its path.

Put the following in your init file:

.. code:: elisp

    (push '("\\.epub\\'" . nov-mode) auto-mode-alist)

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
