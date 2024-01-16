#lang scribble/rhombus/manual

@(import:
    pict open)

@title{Rhombus Pict: Functional Pictures}

@docmodule(pict)

The @rhombusmodname(pict) Rhombus library starts with the same concepts
as the Racket library, but includes direct support for animation. That
is, the @rhombus(Pict) datastype from the @rhombusmodname(pict) library
covers both static pictures, as useful in a paper, and dynamic pictures,
as useful in a slide presentation. In particular, a @rhombus(Pict) can
represent an entire Slideshow talk. Concretely, can pass an animated
pict to a function like @rhombus(above), and the result will be an
animated pict that runs in parallel to other animated picts provided in
the same @rhombus(above) combination.

To make this work, a @rhombus(Pict) is not just a static image that can
be rendered in a drawing context. It's also not just a time-varying
function, because a function by itself does not have enough structure to
usefully compose picts. For example, suppose you want to make a slide
with two bullets, where the first one is revealed at the start of the
presentation, and the second is revealed only after advancing the slide.
You can easily imagine making two pictures: one that draws the first
bullet, and then one that draws both bullets. Furthermore, you can
construct those two pictures easily given two separate bullet pictures,
because the combined pict is just stacking the two vertically.

Imagine, however, that each bullet is meant to fade in as it is
revealed, and each is also meant to fade out as it is dismissed. If you
have a slide before this bullets slide, then advancing to the bullets
slide should fade in the first bullet, then stepping fades in the second
one, then advancing further in the talk fades out both bullets. If you
have the two bullets as time-varying functions, it is possible to create
a new time-varying function, but only with careful arithmetic to change
time numbers for the combined image into suitable relative-time numbers
for the component images.

@rectangle(100, 100, ~fill: "red")
