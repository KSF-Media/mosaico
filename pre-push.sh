#!/usr/bin/env bash

# To use this scirpt: ln -s ./pre-push.sh .git/hooks/pre-push

# set SKIP to the amount of vars to skip from the beginning of local .env
# variables only needed for local dev or only used by clientside mosaico 
# should be defined at the top of .env
SKIP=10
# grab the project name from the current folder
PROJECT_NAME=${PWD##*/}
echo "$PROJECT_NAME: Checking for undefined variables in infra"

# first check to see if infra is checked out
if [[ ! -e ./infra/config.dhall ]]; then
    echo "Please checkout infra before checking for missing variables"
    echo "hint: git submodule update --init"
    exit 2
fi
# Verify that deps are installed; if not, warn and exit.
DEPS=('dhall-to-yaml' 'comm' 'awk')
for DEP in ${DEPS[@]}; do
  if [ -z $(which $DEP) ]; then
    echo "$DEP not found in \$PATH"
    echo "Please install $DEP:"
    exit 2
  fi
done

# define some required vars for dhall
set -a
KSF_ENV="<production|staging|dev>.staging" 
CI_COMMIT_SHA="just needs to be defined"
source .env

mkdir -p testvars
awk -v skip="$SKIP" -F\= '/^[A-Z_]+=/{if (NR <= skip) next;gsub(/"/,"",$2); print "- " $1}' .env > ./testvars/localenv

PAPERS=("hbl" "on" "vn")
for PAPER in ${PAPERS[@]}; do
  dhall-to-yaml <<< '(./infra/services/'$PROJECT_NAME'-'$PAPER'.dhall).envVars' > ./testvars/dhallenv
  MISSING_VARS=$(comm -23 <(sort ./testvars/localenv) <(sort ./testvars/dhallenv))
  if [[ -n $MISSING_VARS ]]; then
      echo "Found some environment variables not defined in infra"
      echo "They need to be added before pushing to remote"
      echo "missing in file: mosaco-$PAPER.dhall"
      echo $MISSING_VARS
      rm -rf ./testvars
      exit 1
  else
      unset $MISSING_VARS
  fi
done

echo "All is well, pushing commits.."