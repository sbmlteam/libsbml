%% Description       : Script for running MATLAB tests
%% Original author(s): Michael Hucka <mhucka@caltech.edu>
%% 
addpath('..')
addpath('../../..')
fail = testBinding
exit(fail)
