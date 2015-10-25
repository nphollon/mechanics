Mechanics
=========

This is a package to help animate physics simulations. You provide it with the
initial conditions and a set of differential equations, and it approximates how
the system changes over time.

[Here is a demo.](https://nphollon.github.io/pendulum.html)

This package does not know any physics. In the example above, the equations for
the pendulum's motion were worked out by hand, and `TimeEvolution.evolve` was
used for animation.
