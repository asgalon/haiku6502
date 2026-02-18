TinyML 8 Bit Experimental Device
================================

Abstract
--------

This is an experiment to see if it makes any sense to shove a minimal neural network model down the throat of an 8 bit CPU.
The emulator is here in 65C02 mode for the better interrupt behaviour that makes it possible to let the cpu 
sleep until some interrupt or reset is triggered.
