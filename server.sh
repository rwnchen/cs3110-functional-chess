#!/bin/bash
ifconfig | grep "inet " | cut -f2 -d' ' | sed -n 2p
