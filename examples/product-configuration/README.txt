Cell phone

Constraints:

    Economy plans don't include any features by default.
    Moderate plans include unlimited weekends.
    Heavy plans include both unlimited weekends and unlimited calls to friends.

    Accessories can only be purchased if their base plan doesn't include
    its feature.

    Single plans only allow one line.  Economy and Moderate shared plans
    allow up to three lines.  Heavy shared plans allow up to five lines.

cell-1.ss: simple cell phone configuration.  Does not restrict the user
interface.

cell-2.ss: restricts the checkboxes if the base plan already includes
features.  Reactively changes the values of the check boxes if the
resulting plan is illegal.



----------------------------------------------------------------------

The bicycle model is a simplification of the Bike.pm that only includes
Pedals, Shoes, Rims, Tires, and Extra accessories.


bike-1.ss: tell the user if a rule is being violated.

bike-2.ss: restrict user choices if a change in an attribute will
violate a rule.

bike-3.ss: automatically adjust 




Simplifications
   Tires don't have a profile



References

Bike.pm configuration description: http://www.itu.dk/research/cla/externals/clib/
