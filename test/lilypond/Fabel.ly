% mm. 1-20 of IV. Fabel, Fantasiestücke Op. 12 (1837) by Robert Schumann (1810-1856).
% Compiled from "test/suihei/Fabel.hei".

\version "2.24.4"

\score {
    \transpose c c {
        \fixed c' {
            \tempo 4 = 120
            \key c \major
            << {\voiceOne << {\voiceOne g4 gis4} \\ {\voiceTwo c2} >>
             << {\voiceOne a8 b8 c'8 d'8} \\ {\voiceTwo < c d >2} >>
             << {\voiceOne e'4 d'4} \\ {\voiceTwo < f g >2} >>
             < e c' >4 r8. e'16} \\ {\voiceTwo < c,, e, >2 < f,, f, >2 < g,, b, >2 < c, g, >4 r4} >>
             << {\voiceOne d'16 < f a >16 r16 < f a >16 b16 < f g >16 r16 < f g >16 c'16 < e g >16 r16 < e g >16 gis16 < d e >16 r16 < d e >16 a16 < c e >16 r16 < c e >16 fis16 < c d >16 r16 < c d >16 g16 < b, d >16 g16 < c e >16 g16 < b, d >16 r16 e'16 d'16 < f a >16 r16 < f a >16 b16 < f g >16 r16 < f g >16 c'16 < e g >16 r16 c'16 c''16 < c' gis' >16 r16 gis'16 a'16 < a d' >16 r16 < a d' >16 f'16 < f b >16 r16 < f b >16 c'16 < e g >16 c'16 < f a >16 c'16 < e g >16 r8} \\ {\voiceTwo r16 c,16 < a, d >16 c,16 r16 c,16 < g, d >16 c,16 r16 c,16 < g, c >16 c,16 r16 e,16 < gis, b, >16 e,16 r16 a,,16 < e, a, >16 a,,16 r16 d,,16 < fis, a, >8 r16 g,,16 < d, g, >16 g,,16 < e, g, >16 g,,16 < d, g, >16 g,,16 r16 c,16 < a, d >16 c,16 r16 c,16 < g, d >16 c,16 r16 c,16 < g, c >16 c,16 r16 e,16 < bes, gis >8 r16 f,16 < a, f >16 f,16 r16 g,16 < b, d >16 g,16 r16 c,16 < g, c >16 c,16 < a, c >16 c,16 < g, c >16 c,16} >>
             << {\voiceOne << {\voiceOne d'4 dis'4} \\ {\voiceTwo g2} >>
             << {\voiceOne e'8 fis'8 g'8 a'8} \\ {\voiceTwo < g a >2} >>
             << {\voiceOne b'4 a'4} \\ {\voiceTwo < c' d' >2} >>
             << {\voiceOne g'4 fis'8 e'8} \\ {\voiceTwo b2} >>
             << {\voiceOne d'4 dis'4} \\ {\voiceTwo b4 a4} >>
             << {\voiceOne e'8 fis'8 g'8 a'8} \\ {\voiceTwo g2} >>
             << {\voiceOne b'4 a'4} \\ {\voiceTwo < c' d' >2} >>
             < b g' >2} \\ {\voiceTwo < g,, b, >2 < c, c >2 << {\voiceOne fis4 d2 e8 fis8 a4 fis4 e4 ees4} \\ {\voiceTwo d,2 g,1 c,2} >>
             << {\voiceOne d4 fis4} \\ {\voiceTwo d,4 d4} >>
             < g, d >2} >>

        }
    }
    \layout { }
    \midi { }
}