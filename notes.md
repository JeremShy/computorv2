Must transform RPN into an AST

(1 + 2) * (3 - 4) = 12+34-*

Resultat attendu :

                  *
         +               -
     1       2        3      4


12+34-*

Ici, on peut combiner x et y s'ils sont calculables, ainsi que x et z et z et y
              +
      x               +
                  z      y


Deux noeuds sont calculables si:

. Dans le cas d'une addition ou d'une soustraction Factorisation : a * b + c * b = (a + c) * b
  - Multiple de x^0 = 1, par exemple 1 + 2
  - Multiple de x^1 = x, par exemple 2x + 3x
  - Multiple de x^2 = x^2, par exemple 2x^2 - x^2


3 - 4
1 + 2



# Division
$$
ac + bd + i(bc - ad)
\over
c^2 + d^2
$$

$$
\frac {ac + bd} {c^2 + d^2}
+
i  \frac {bc - ad} {c^2 + d^2}
$$

#Multiplication
$$
	\begin{split}
	(a + bi) * (c + di) & = ac + adi + bci + bdi^2 \\
		& = ac + i(ad + bc) - bd \\
		& = (ac - bd) + i(ad + bc)
	\end{split}

$$


#Power
$$
	\forall n \in \mathbb{N}, (a + bi)^n = (a + bi) * (a + bi)^{(n-1)}
$$

#### Power example
$$
	\begin{split}
	(1 + i)^2	& = 1^2 + 2i + i^2 \\
 				& = 1 + 2i - 1 \\
				& = 2i
	\end{split}
$$

$$
	\begin{split}
	(1 + 2i)^3	& = (1 + 2i)^2 * (1 + 2i) \\
				& = (1^2 + 4i + (2i)^2) * (1 + 2i) \\
				& = (1 + 4i - 4) * (1 + 2i) \\
				& = (-3 + 4i) * (1 + 2i) \\
				& = (-3 - 6i + 4i - 8) \\
				& = (-11 - 2i)
	\end{split}
$$
