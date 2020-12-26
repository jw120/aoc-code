#!/bin/sh

for fullname in exec/day*_run.R; do
  filename=${fullname#exec/day}
  num=${filename%_run.R}
  echo Running day ${num}
  rm -f outputs/${num}.txt
  ${fullname}
  diff outputs/${num}.txt good_outputs/${num}.txt && echo OK
done

