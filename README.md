Family Tree (Racket Coursework)

A short Racket / Scheme coursework project from my first year at university that stores a small family tree (maternal + paternal branches) and runs a set of queries on it (living members, ages, sorting, renaming, etc.).

Project file:

What it does

Defines two branches of a family tree as lists:

Mb (Maternal branch)

Pb (Paternal branch)

Each person is stored as: Name, Mother, Father, Date of Birth, Date of Death (empty list means unknown/still alive)

Implements functions to:

List all members in each branch (C1, C2) and both combined (C3)

Get parents, living members, and current ages (A1–A3)

Find people with the same birthday month (A4 / B4)

Sort by last name (Mb) / first name (Pb) (A5 / B5)

Rename matching first names (e.g., John → Juan, Mary → Maria) (A6 / B6)

Find oldest living member and average age at death (B2 / B3)

How to run

Open the file in DrRacket and click Run
(The program prints the results for each task to the console.)
