## Allgemeines

Dieses Projekt implementiert einen Sudoku Solver auf SMT Basis unter Verwendung von "hasmtlib" und Haskell als Hostsprache.
## SMT Spezifikationen:

- Solver: Z3
- Logik: QF_LIA - linear integer arithmetic
## Implementierung

Sudoku basiert auf einem 9x9 Feld
````haskell
board <- replicateM n $ replicateM n $ var @IntSort
````

Bereits belegte Felder werden als Array von Tupeln dargestellt, welche jeweils die Position und den Wert des Feldes enthalten:
````haskell
let setCells = [(n, (row,column)),...]
````

Das Rätsel hat 3 Regeln:
- Jede Zeile muss genau die Zahlen 1-9 enthalten (keine Doppelungen)
- Jede Spalte muss genau die Zahlen 1-9 enthalten (keine Doppelungen)
- Jedes 3x3 muss genau die Zahlen 1-9 enthalten (keine Doppelungen)

Diese wurden im Wesentlichen durch folgende 3 asserts implementiert:

````haskell
-- row constraint
forM_ board $ \row -> do
  assert $ distinct row

-- column constraint
forM_ (transpose board) $ \column -> do
  assert $ distinct column

-- subgrid constraint
let subgrids = extractSubgrids board
forM_ subgrids $ \subgrid -> do
  assert $ distinct subgrid
````
Distinct ist hierbei eine Funktion der hasmtlib Bibliothek.

Zum Erstellen des "3x3 Subgrids" gibt es zwei wesentliche Funktionen:

````haskell
getSubgridAtPosition :: [[Expr 'IntSort]] -> Int -> Int -> [Expr 'IntSort]
getSubgridAtPosition board row column = concatMap (take 3 . drop column) (take 3 (drop row board))

getAllSubgrids :: [[Expr 'IntSort]] -> [[Expr 'IntSort]]
getAllSubgrids board = concatMap (\row -> map (\column -> getSubgridAtPosition board row column) [0, 3, 6]) [0, 3, 6]
````

``getSubgridAtPosition board row column`` filtert ein 3x3 Grid, beginnend an den Argumenten "row" und "grid". Die mittlere Subgrid würde beispielsweise durch den Aufruf ``getSubgridAtPosition board 3 3`` gefiltert werden.

``getAllSubgrids board`` wendet diese Funktion auf die 9 3x3 Felder im Sudoku an. Also wie folgt:
  - (0,0), (0,3), (0,6)
  - (3,0), (3,3), (3,6)
  - (6,0), (6,3), (6,6)

Hinzukommen 2 Constraints, welche den Zahlenraum einschränken (1 bis 9) und n Constraints für die bereits ausgefüllten Felder

````haskell
-- assert correct boundries for numbers
forM_ (concat board) $ assert . (>? 0)
forM_ (concat board) $ assert . (<? fromIntegral n+1)

-- assert set numbers
forM_ setCells $ \cell -> do
  let row = fst(snd cell)
  let column = snd(snd cell)
  let n = fst cell
  assert $ ((board !! row) !! column) === n
````

## Beispiel

Als Beispiel wird folgendes Sudoku betrachtet:
- https://www.chiark.greenend.org.uk/~sgtatham/puzzles/js/solo.html#3x3:b9_8a5a4_7b3d8a5a2a9d3b7c9k7c1b6d7a6a2a1d9b8_9a1a2_3b

Die bereits belegten Felder werden dem Programm wie folgt übergeben:
````haskell
let setNumbers = [(9, (0,2)),(8, (0,3)),(5, (0,5)),(4, (0,7)),(7, (0,8)), (3, (1,2)),(8, (1,7)), (5, (2,0)),(2, (2,2)),(9, (2,4)),(3, (3,0)),(7, (3,3)),(9, (3,7)),(7, (5,1)),(1, (5,5)),(6, (5,8)),(7, (6,4)),(6, (6,6)),(2, (6,8)),(1, (7,1)),(9, (7,6)),(8, (8,0)),(9, (8,1)),(1, (8,3)),(2, (8,5)),(3, (8,6))]
````

Hasmtlib baut daraus folgendes SMT Modell:
````haskell
(set-option :produce-models true)
(set-logic QF_LIA)
(declare-fun var_1 () Int)
(declare-fun var_2 () Int)
(declare-fun var_3 () Int)
(declare-fun var_4 () Int)
(declare-fun var_5 () Int)
(declare-fun var_6 () Int)
(declare-fun var_7 () Int)
(declare-fun var_8 () Int)
(declare-fun var_9 () Int)
(declare-fun var_10 () Int)
(declare-fun var_11 () Int)
(declare-fun var_12 () Int)
(declare-fun var_13 () Int)
(declare-fun var_14 () Int)
(declare-fun var_15 () Int)
(declare-fun var_16 () Int)
(declare-fun var_17 () Int)
(declare-fun var_18 () Int)
(declare-fun var_19 () Int)
(declare-fun var_20 () Int)
(declare-fun var_21 () Int)
(declare-fun var_22 () Int)
(declare-fun var_23 () Int)
(declare-fun var_24 () Int)
(declare-fun var_25 () Int)
(declare-fun var_26 () Int)
(declare-fun var_27 () Int)
(declare-fun var_28 () Int)
(declare-fun var_29 () Int)
(declare-fun var_30 () Int)
(declare-fun var_31 () Int)
(declare-fun var_32 () Int)
(declare-fun var_33 () Int)
(declare-fun var_34 () Int)
(declare-fun var_35 () Int)
(declare-fun var_36 () Int)
(declare-fun var_37 () Int)
(declare-fun var_38 () Int)
(declare-fun var_39 () Int)
(declare-fun var_40 () Int)
(declare-fun var_41 () Int)
(declare-fun var_42 () Int)
(declare-fun var_43 () Int)
(declare-fun var_44 () Int)
(declare-fun var_45 () Int)
(declare-fun var_46 () Int)
(declare-fun var_47 () Int)
(declare-fun var_48 () Int)
(declare-fun var_49 () Int)
(declare-fun var_50 () Int)
(declare-fun var_51 () Int)
(declare-fun var_52 () Int)
(declare-fun var_53 () Int)
(declare-fun var_54 () Int)
(declare-fun var_55 () Int)
(declare-fun var_56 () Int)
(declare-fun var_57 () Int)
(declare-fun var_58 () Int)
(declare-fun var_59 () Int)
(declare-fun var_60 () Int)
(declare-fun var_61 () Int)
(declare-fun var_62 () Int)
(declare-fun var_63 () Int)
(declare-fun var_64 () Int)
(declare-fun var_65 () Int)
(declare-fun var_66 () Int)
(declare-fun var_67 () Int)
(declare-fun var_68 () Int)
(declare-fun var_69 () Int)
(declare-fun var_70 () Int)
(declare-fun var_71 () Int)
(declare-fun var_72 () Int)
(declare-fun var_73 () Int)
(declare-fun var_74 () Int)
(declare-fun var_75 () Int)
(declare-fun var_76 () Int)
(declare-fun var_77 () Int)
(declare-fun var_78 () Int)
(declare-fun var_79 () Int)
(declare-fun var_80 () Int)
(declare-fun var_81 () Int)
(assert (> var_1 0))
(assert (> var_2 0))
(assert (> var_3 0))
(assert (> var_4 0))
(assert (> var_5 0))
(assert (> var_6 0))
(assert (> var_7 0))
(assert (> var_8 0))
(assert (> var_9 0))
(assert (> var_10 0))
(assert (> var_11 0))
(assert (> var_12 0))
(assert (> var_13 0))
(assert (> var_14 0))
(assert (> var_15 0))
(assert (> var_16 0))
(assert (> var_17 0))
(assert (> var_18 0))
(assert (> var_19 0))
(assert (> var_20 0))
(assert (> var_21 0))
(assert (> var_22 0))
(assert (> var_23 0))
(assert (> var_24 0))
(assert (> var_25 0))
(assert (> var_26 0))
(assert (> var_27 0))
(assert (> var_28 0))
(assert (> var_29 0))
(assert (> var_30 0))
(assert (> var_31 0))
(assert (> var_32 0))
(assert (> var_33 0))
(assert (> var_34 0))
(assert (> var_35 0))
(assert (> var_36 0))
(assert (> var_37 0))
(assert (> var_38 0))
(assert (> var_39 0))
(assert (> var_40 0))
(assert (> var_41 0))
(assert (> var_42 0))
(assert (> var_43 0))
(assert (> var_44 0))
(assert (> var_45 0))
(assert (> var_46 0))
(assert (> var_47 0))
(assert (> var_48 0))
(assert (> var_49 0))
(assert (> var_50 0))
(assert (> var_51 0))
(assert (> var_52 0))
(assert (> var_53 0))
(assert (> var_54 0))
(assert (> var_55 0))
(assert (> var_56 0))
(assert (> var_57 0))
(assert (> var_58 0))
(assert (> var_59 0))
(assert (> var_60 0))
(assert (> var_61 0))
(assert (> var_62 0))
(assert (> var_63 0))
(assert (> var_64 0))
(assert (> var_65 0))
(assert (> var_66 0))
(assert (> var_67 0))
(assert (> var_68 0))
(assert (> var_69 0))
(assert (> var_70 0))
(assert (> var_71 0))
(assert (> var_72 0))
(assert (> var_73 0))
(assert (> var_74 0))
(assert (> var_75 0))
(assert (> var_76 0))
(assert (> var_77 0))
(assert (> var_78 0))
(assert (> var_79 0))
(assert (> var_80 0))
(assert (> var_81 0))
(assert (< var_1 10))
(assert (< var_2 10))
(assert (< var_3 10))
(assert (< var_4 10))
(assert (< var_5 10))
(assert (< var_6 10))
(assert (< var_7 10))
(assert (< var_8 10))
(assert (< var_9 10))
(assert (< var_10 10))
(assert (< var_11 10))
(assert (< var_12 10))
(assert (< var_13 10))
(assert (< var_14 10))
(assert (< var_15 10))
(assert (< var_16 10))
(assert (< var_17 10))
(assert (< var_18 10))
(assert (< var_19 10))
(assert (< var_20 10))
(assert (< var_21 10))
(assert (< var_22 10))
(assert (< var_23 10))
(assert (< var_24 10))
(assert (< var_25 10))
(assert (< var_26 10))
(assert (< var_27 10))
(assert (< var_28 10))
(assert (< var_29 10))
(assert (< var_30 10))
(assert (< var_31 10))
(assert (< var_32 10))
(assert (< var_33 10))
(assert (< var_34 10))
(assert (< var_35 10))
(assert (< var_36 10))
(assert (< var_37 10))
(assert (< var_38 10))
(assert (< var_39 10))
(assert (< var_40 10))
(assert (< var_41 10))
(assert (< var_42 10))
(assert (< var_43 10))
(assert (< var_44 10))
(assert (< var_45 10))
(assert (< var_46 10))
(assert (< var_47 10))
(assert (< var_48 10))
(assert (< var_49 10))
(assert (< var_50 10))
(assert (< var_51 10))
(assert (< var_52 10))
(assert (< var_53 10))
(assert (< var_54 10))
(assert (< var_55 10))
(assert (< var_56 10))
(assert (< var_57 10))
(assert (< var_58 10))
(assert (< var_59 10))
(assert (< var_60 10))
(assert (< var_61 10))
(assert (< var_62 10))
(assert (< var_63 10))
(assert (< var_64 10))
(assert (< var_65 10))
(assert (< var_66 10))
(assert (< var_67 10))
(assert (< var_68 10))
(assert (< var_69 10))
(assert (< var_70 10))
(assert (< var_71 10))
(assert (< var_72 10))
(assert (< var_73 10))
(assert (< var_74 10))
(assert (< var_75 10))
(assert (< var_76 10))
(assert (< var_77 10))
(assert (< var_78 10))
(assert (< var_79 10))
(assert (< var_80 10))
(assert (< var_81 10))
(assert (= var_3 9))
(assert (= var_4 8))
(assert (= var_6 5))
(assert (= var_8 4))
(assert (= var_9 7))
(assert (= var_12 3))
(assert (= var_17 8))
(assert (= var_19 5))
(assert (= var_21 2))
(assert (= var_23 9))
(assert (= var_28 3))
(assert (= var_31 7))
(assert (= var_35 9))
(assert (= var_47 7))
(assert (= var_51 1))
(assert (= var_54 6))
(assert (= var_59 7))
(assert (= var_61 6))
(assert (= var_63 2))
(assert (= var_65 1))
(assert (= var_70 9))
(assert (= var_73 8))
(assert (= var_74 9))
(assert (= var_76 1))
(assert (= var_78 2))
(assert (= var_79 3))
(assert (distinct var_3 var_4 var_5 var_6 var_7 var_8 var_9 var_1 var_2))
(assert (distinct var_12 var_13 var_14 var_15 var_16 var_17 var_18 var_10 var_11))
(assert (distinct var_21 var_22 var_23 var_24 var_25 var_26 var_27 var_19 var_20))
(assert (distinct var_30 var_31 var_32 var_33 var_34 var_35 var_36 var_28 var_29))
(assert (distinct var_39 var_40 var_41 var_42 var_43 var_44 var_45 var_37 var_38))
(assert (distinct var_48 var_49 var_50 var_51 var_52 var_53 var_54 var_46 var_47))
(assert (distinct var_57 var_58 var_59 var_60 var_61 var_62 var_63 var_55 var_56))
(assert (distinct var_66 var_67 var_68 var_69 var_70 var_71 var_72 var_64 var_65))
(assert (distinct var_75 var_76 var_77 var_78 var_79 var_80 var_81 var_73 var_74))
(assert (distinct var_19 var_28 var_37 var_46 var_55 var_64 var_73 var_1 var_10))
(assert (distinct var_20 var_29 var_38 var_47 var_56 var_65 var_74 var_2 var_11))
(assert (distinct var_21 var_30 var_39 var_48 var_57 var_66 var_75 var_3 var_12))
(assert (distinct var_22 var_31 var_40 var_49 var_58 var_67 var_76 var_4 var_13))
(assert (distinct var_23 var_32 var_41 var_50 var_59 var_68 var_77 var_5 var_14))
(assert (distinct var_24 var_33 var_42 var_51 var_60 var_69 var_78 var_6 var_15))
(assert (distinct var_25 var_34 var_43 var_52 var_61 var_70 var_79 var_7 var_16))
(assert (distinct var_26 var_35 var_44 var_53 var_62 var_71 var_80 var_8 var_17))
(assert (distinct var_27 var_36 var_45 var_54 var_63 var_72 var_81 var_9 var_18))
(assert (distinct var_3 var_10 var_11 var_12 var_19 var_20 var_21 var_1 var_2))
(assert (distinct var_6 var_13 var_14 var_15 var_22 var_23 var_24 var_4 var_5))
(assert (distinct var_9 var_16 var_17 var_18 var_25 var_26 var_27 var_7 var_8))
(assert (distinct var_30 var_37 var_38 var_39 var_46 var_47 var_48 var_28 var_29))
(assert (distinct var_33 var_40 var_41 var_42 var_49 var_50 var_51 var_31 var_32))
(assert (distinct var_36 var_43 var_44 var_45 var_52 var_53 var_54 var_34 var_35))
(assert (distinct var_57 var_64 var_65 var_66 var_73 var_74 var_75 var_55 var_56))
(assert (distinct var_60 var_67 var_68 var_69 var_76 var_77 var_78 var_58 var_59))
(assert (distinct var_63 var_70 var_71 var_72 var_79 var_80 var_81 var_61 var_62))
(check-sat)
(get-model)
````

Z3 berechnet daraus folgendes Ergebnis:
````haskell
(Sat,Just [[1,6,9,8,3,5,2,4,7],[7,4,3,2,1,6,5,8,9],[5,8,2,4,9,7,1,6,3],[3,2,1,7,6,4,8,9,5],[6,5,4,3,8,9,7,2,1],[9,7,8,5,2,1,4,3,6],[4,3,5,9,7,8,6,1,2],[2,1,7,6,4,3,9,5,8],[8,9,6,1,5,2,3,7,4]])
````
Ein Array entspricht dabei einer Zeile
