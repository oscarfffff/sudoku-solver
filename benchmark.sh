#!/bin/bash

# Anzahl der Durchläufe
RUNS=10

# Cabal-Befehl
PROGRAM="cabal run"

# Variablen zum Speichern der Gesamtzeit
total_real=0
total_user=0
total_sys=0

# Schleife für die Ausführungen
for ((i = 1; i <= RUNS; i++)); do
  echo "Durchlauf $i:"

  # Führe das Programm mit `time` aus und speichere die Zeiten
  output=$( (time $PROGRAM) 2>&1 )
  echo "$output"

  # Extrahiere die Zeiten (real, user, sys)
  real_time=$(echo "$output" | grep real | awk '{print $2}')
  user_time=$(echo "$output" | grep user | awk '{print $2}')
  sys_time=$(echo "$output" | grep sys | awk '{print $2}')

  # Konvertiere Zeit in Sekunden (Minuten und Sekunden)
  real_sec=$(echo $real_time | awk -Fm '{print $1*60 + $2}')
  user_sec=$(echo $user_time | awk -Fm '{print $1*60 + $2}')
  sys_sec=$(echo $sys_time | awk -Fm '{print $1*60 + $2}')

  # Addiere die Zeiten zur Gesamtzeit
  total_real=$(echo "$total_real + $real_sec" | bc)
  total_user=$(echo "$total_user + $user_sec" | bc)
  total_sys=$(echo "$total_sys + $sys_sec" | bc)
done

# Durchschnitt berechnen
average_real=$(echo "$total_real / $RUNS" | bc -l)
average_user=$(echo "$total_user / $RUNS" | bc -l)
average_sys=$(echo "$total_sys / $RUNS" | bc -l)

# Ergebnisse ausgeben
echo "--------------------------------"
echo "Durchschnittliche Zeiten über $RUNS Durchläufe:"
echo "Real: $average_real Sekunden"
echo "User: $average_user Sekunden"
echo "Sys: $average_sys Sekunden"
