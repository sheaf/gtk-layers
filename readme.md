# `gtk-layers`

This project demonstrates how to use the `haskell-gi` GTK 4 bindings to
implement a system of layers, like one finds in typical image editing software
such as Photoshop, Inkscape or GIMP.

![demo animation](gtk-layers-demo.gif)

## Running

To run, clone the repository and run `cabal run exe:gtk-layers`.

This requires the GTK 4 C library to be installed, version 4.10 or above.
See the [`haskell-gi` repository](https://github.com/haskell-gi/haskell-gi)
for installation instructions for various platforms.

## Overview

The `gtk-layers` executable demonstrates key some implementation details of a
layer system in GTK:

  - The GTK 4 `TreeListModel` API, which separates the underlying model
    hierarchy from its visual display (it replaces the now deprecated `TreeView` API).  
    In particular, the application illustrates:
      - how to create a `TreeListModel` with child models, avoiding common
        pitfalls;
      - how a `TreeListModel` can be edited by keeping references to underlying
        child list stores;
      - how to store custom data in a list model, and what data to choose to
        facilitate editing the model;
      - how to set up the `ListView` that displays the model, including what
        to do in the `setup` and `bind` signal handlers of the `SignalListItemFactory`
        which is used to create widgets that visualise the underlying data.

  - The GTK drag-and-drop API, which includes:
      - how to communicate Haskell data from the drag source to the drop target;
      - full CSS styling to animate the drag and drop effect, using whether
        one is hovering on the top of bottom of a layer to style the destination
        differently;
      - updating the selection after a move operation.

  - Undo/redo functionality.

  - Other details such as the handling of ownership or the use of STM to
    ensure atomicity of updates.

The code contains extensive comments that detail the implementation and justify
the approach taken.

## Acknowledgements

I extend my thanks to Iñaki García Etxebarria, the author and maintainer of the
`haskell-gi` library, for his patient help with diagnosing and fixing bugs in
early versions of this project.


