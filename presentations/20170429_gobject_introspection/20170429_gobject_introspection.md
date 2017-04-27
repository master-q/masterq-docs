# Introduction of GObject Introspection

Kiwamu Okabe

# What's GTK+?

https://www.gtk.org/

* A cross-platform widget toolkit for creating GUI
* Written in C language

![inline](img/1024px-GTK+_software_architecture.svg.png)

# Example application: GIMP

https://www.gimp.org/

![inline](img/gimp.png)

# What's GLib?

https://wiki.gnome.org/Projects/GLib

* A part of GTK+
* A low-level system library to provide advanced data structures and threads
* Written in C language

# Example application: GNOME

https://www.gnome.org/

![inline](img/1280px-Gnome-3.18.2-showing-overview.png)

# Optimistically using malloc(3)
# What's GObject?

https://developer.gnome.org/gobject/stable/

* A part of GLib
* An object system includes type system GType
* Written in C language

# Example class: GtkProgressBar

https://developer.gnome.org/gtk3/stable/GtkProgressBar.html

```
GObject
-> GInitiallyUnowned
   -> GtkWidget
      -> GtkProgressBar
```

![inline](img/progressbar.png)

# Example definition: GtkProgressBar
# Example constructor: GtkProgressBar

```
GtkWidget *gtk_progress_bar_new()
=> g_object_new((GType) GTK_TYPE_PROGRESS_BAR, NULL)
   => g_object_new_with_properties((GType) GTK_TYPE_PROGRESS_BAR, 0,
                                   NULL, NULL)
      => GObjectClass *class =
	       g_type_class_peek_static((GType) GTK_TYPE_PROGRESS_BAR)
         => TypeNode *node = lookup_type_node_I(type)
         => return node->data->class.class
      => return (GObject *) g_object_new_internal(class, NULL, 0)
         => return (GObject *) g_type_create_instance(
		                         class->g_type_class.g_type)
```

# What's GObject Introspection?

* A middleware layer between C libraries (using GObject) and language bindings
* Such languages are:

```
JavaScript
Python
Perl
Java
Lua
.NET
Scheme
etc.
```

# What's haskell-gi?
