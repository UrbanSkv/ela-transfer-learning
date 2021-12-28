%Calculates the landscape features of the COCO problems that will be used to calculate the ELA features
%Based on the code available at https://github.com/numbbo/coco/
%Copyright (c) 2013-2018 by the NumBBO/CoCO team. See AUTHORS_COCO file and
%remarks below for exceptions and more details.
%
%Apart from the below mentioned exceptions, this code is distributed under 
%the 3-clause BSD License. 
%
%### 3-clause BSD License ###
%
%Redistribution and use in source and binary forms of the software as well
%as documentation, with or without modification, are permitted provided
%that the following conditions are met:
%
%1. Redistributions of source code must retain the above copyright
%  notice, this list of conditions and the following disclaimer.
%
%2. Redistributions in binary form must reproduce the above
%  copyright notice, this list of conditions and the following
%  disclaimer in the documentation and/or other materials provided
%  with the distribution.
%
%3. The names of the contributors may not be used to endorse or
%  promote products derived from this software without specific
%  prior written permission.
%
%THIS SOFTWARE AND DOCUMENTATION IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT
%NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
%OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
%EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
%PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%SOFTWARE AND DOCUMENTATION, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%DAMAGE.

%Note that some parts of the provided software have been written
%by other contributors outside of the NumBBO/CoCO team as indicated
%in AUTHORS and therefore covered by different licenses than the above.
%We acknowledge in particular:
%* the AVL tree code by Michael H. Buselli and Wessel Dankers
%  in ``code-experiments/src/mo_avl_tree.c``, distributed under the terms
%  of the GNU Lesser General Public License
%* the hypervolume code of the SMS-EMOA from the Shark library in
%  ``code-experiments/examples/bbob-biobj-matlab-smsemoa/``, distributed
%  under the terms of the GNU General Public License and the non-dominated
%  sorting part as written by Y. Cao
%* the MinUnit code used as the basis for the implementation in 
%  ``code-experiments/test/unit-test/minunit_c89.h``, distributed under
%  the terms of the MIT Licence.


%Gets samples from the coco_problems which will be used to calculate the landscape problems.
for pop_size= [50, 100, 200, 250]
%for pop_size= 50
    for iter = 1:10
        for dimension = [10, 20, 40] % doesn't let me do more than 40
        %for dimension = [2 10 30 50] 
            for  sample_type = 0:4
more off; % to get immediate output in Octave

%%%%%%%%%%%%%%%%%%%%%%%%%
% Experiment Parameters %
%%%%%%%%%%%%%%%%%%%%%%%%%
BUDGET_MULTIPLIER = 1000000; % algorithm runs for BUDGET_MULTIPLIER*dimension funevals
NUM_OF_INDEPENDENT_RESTARTS = 0; % max. number of independent algorithm
% restarts; if >0, make sure that the
% algorithm is not always doing the same thing
% in each run (which is typically trivial for
% randomized algorithms)

%%%%%%%%%%%%%%%%%%%%%%%%%
% Prepare Experiment    % 
%%%%%%%%%%%%%%%%%%%%%%%%%

% choose a test suite and a matching logger, for
% example one of the following:
%
% bbob              24 unconstrained noiseless single-objective functions
% bbob-biobj        55 unconstrained noiseless bi-objective functions
% bbob-biobj-ext    92 unconstrained noiseless bi-objective functions
% bbob-largescale   24 unconstrained noiseless single-objective functions in large dimensions
% bbob-constrained  48 constrained noiseless single-objective functions
%
suite_name = 'bbob';
observer_name = 'bbob_observer';
observer_options = strcat('result_folder: RS_on_', ...
    suite_name, ...
    [' algorithm_name: RS '...
    ' algorithm_info: A_simple_random_search ']);

% initialize suite and observer with default options,
% to change the default, see 
% http://numbbo.github.io/coco-doc/C/#suite-parameters and
% http://numbbo.github.io/coco-doc/C/#observer-parameters
% for details.
%dimension=30;
% " function_indices: 1-24 "
suite_string = strcat("dimensions: ", string(dimension), " function_indices: 1-24 instance_indices: 1");
suite_string = convertStringsToChars(suite_string); %to zgleda da mora biti char array
disp(suite_string);
%suite = cocoSuite(suite_name, '', 'dimensions: 10 function_indices: 1-24');
suite = cocoSuite(suite_name, '', suite_string);
observer = cocoObserver(observer_name, observer_options);
%observer = cocoObserver(observer_name, "");
% set log level depending on how much output you want to see, e.g. 'warning'
% for fewer output than 'info'.
cocoSetLogLevel('info');

% keep track of problem dimension and #funevals to print timing information:
printeddim = 1;
doneEvalsAfter = 0; % summed function evaluations for a single problem
doneEvalsTotal = 0; % summed function evaluations per dimension
printstring = '\n'; % store strings to be printed until experiment is finished



%%%%%%%%%%%%%%%%%%%%%%%%%
% Run Experiment        %
%%%%%%%%%%%%%%%%%%%%%%%%%
all_samples = cell(1,24);
all_results = cell(1,24);
i=0;

%dimension spremeni zgoraj (v suite)
nsamples = pop_size * dimension;
%samples = lhsdesign(nsamples,dimension);
samples = sample_generator(dimension, nsamples, sample_type);
scale_factor = 10;
scaled_samples = samples * scale_factor - scale_factor/2;
% Each function has 15 instances
while true
    i = i + 1;
    
    % get next problem and dimension from the chosen suite:
    problem = cocoSuiteGetNextProblem(suite, observer);
    if ~cocoProblemIsValid(problem)
        break;
    end
    disp(cocoProblemGetName(problem));
    dimension = double(cocoProblemGetDimension(problem));
    
    results = [];
    for row = scaled_samples.'
        y = cocoEvaluateFunction(problem,row);
        results = [results; y];
    end
    
    all_samples{i}=scaled_samples;
    all_results{i}=results;
end


all_results_mat_200 = cat(1,all_results{:});
all_samples_mat_200 = cat(1,all_samples{:});

save(strcat("E:\\samples_30_50\\all_samples_mat_",string(iter),"_sampling_",string(sample_type),"_coco_d",string(dimension),"_",string(pop_size),".mat"), "all_samples_mat_200")
save(strcat("E:\\samples_30_50\\all_results_mat_",string(iter),"_sampling_",string(sample_type),"_coco_d",string(dimension),"_",string(pop_size),".mat"), "all_results_mat_200")

disp(strcat("SAVED: all_results_mat_",string(iter),"_sampling_",string(sample_type),"_coco_d",string(dimension),"_",string(pop_size)))
        
cocoObserverFree(observer);
cocoSuiteFree(suite);

            end
        end
    end
end
