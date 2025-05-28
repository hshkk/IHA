% mm. 1-6 of Praeludium I (BWV 846) from Das Wohltemperirte Clavier I by Johann Sebastian Bach (1685-1750).

\version "2.24.4"

\header {
  title    = "Das Wohltemperirte Clavier I"
  subtitle = "Praeludium I"
  composer = "Johann Sebastian Bach (1685-1750)"
  opus     = "BWV 846"
}

\score {
  \layout {
    indent = 0.0
  }
  
  \parallelMusic vA, vB, vC {
    % Measure 1
    r8  g,16 c e g, c e r8  g,16 c e g, c e         |
    r16 e8.~   4        r16 e8.~   4                |
    c2                  c                           |
    % Measure 2
    r8  a,16 d f a, d f r8  a,16 d f a, d f         |
    r16 d8.~   4        r16 d8.~   4        	    |
    c2                  c                           |
    % Measure 3
    r8  g,16 d' f g, d' f r8  g,16 d' f g, d' f     |
    r16 d8.~    4         r16 d8.~    4             |
    b2                    b                         |
    % Measure 4
    r8  g,16 c e g, c e r8  g,16 c e g, c e         |
    r16 e8.~   4        r16 e8.~  4                 |
    c2                  c                           |
    % Measure 5
    r8  a,16 e' a a, e' a r8  a,16 e' a a, e' a     |
    r16 e8.~    4         r16 e8.~     4            |
    c2                    c                         |
    % Measure 6
    r8  fis,16 a d fis, a d r8  fis,16 a d fis, a d |
    r16 d8.~     4          r16 d8.~     4          |
    c2                      c                       |
  }

  \new PianoStaff <<
    \new Staff << 
      \relative c''' \vA 
    >>
    \new Staff << 
      \clef bass \relative c' \vB \\ \relative c' \vC 
    >>
  >>
}