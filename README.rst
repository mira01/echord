# Chord implementation in Erlang

Chord is an application and file format for creating songsheetets

This is hobby project which main purpose is learn Erlang

Current status: Program can read file test.chord, parse it to inner represention and produce html output

To be done in upcoming days:
    
    - parsing of chords
    - distinction of chorus verses and recitative verses

Features
    
    - Parse chord files (90% done)
    - Format output
        * html (OK)
        * Pdf (Planned)
    - Optimized sorting - save space by finding two short songs and placing them to one page instead of two separate (Planned)
