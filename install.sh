#!/bin/bash

export GPG_TTY=$(tty)
mvn install
