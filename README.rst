*********
clon, WIP
*********

A programming language ressembling lua - `do`, `then` and `elseif` + some other syntax salad. Making it because current languages all lack some part of what I need or do it in a bad way (C++).

Syntax
======

If
--

.. code-block:: lua

   if
   ?(cond1)
      body1;
   ?(cond2)
      body2;
   ?()
      else_body;
   end

For
---

.. code-block:: lua

   for (loc i:int = 0; i < N; i = i + 1)
      body;
   end

For..In
-------

.. code-block:: lua

   for (item in array)
      body;
   end

Variables
---------

.. code-block:: lua

   loc name:string;
   loc name:string = "Mr. Boombastic";

Functions
---------

Expressions

.. code-block:: lua

   (fc(arg1:typ1, arg2:typ2, ...): ret_type
      body;
   end)

*Can't assign functions to variables because function type parsing not yet implemented*

Statements

.. code-block:: lua

   fc add(a:int, b:int): int
      ret a + b;
   end
