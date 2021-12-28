%Determines the best performing algorithm on each COCO problem

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

addpath('Optimizer');
Algorithm = {@ABC,@ACO,@CMAES,@CSO,@DE,@FEP,@GA,@PSO,@SA,@Rand};
nruns = 10;
%cols: algorithms, rows= runs
Data  = zeros(nruns,length(Algorithm));
allBest = zeros(24*15,10);
currentFunction = 1;
%for ifun = benchmarks('FunctionIndices')  % or benchmarksnoisy(...)
for ifun = 1:24  % or benchmarksnoisy(...)
    for iinstance = 1:15
        %disp([ifun iinstance]);
        for currentAlgorithm = 1:length(Algorithm)
            addpath('.');  % should point to fgeneric.m etc.
            datapath = '.';  % different folder for each experiment
            % opt.inputFormat = 'row';
            opt.algName = 'PUT ALGORITHM NAME';
            opt.comments = 'PUT MORE DETAILED INFORMATION, PARAMETER SETTINGS ETC';
            maxfunevals = '10 * dim'; % 10*dim is a short test-experiment taking a few minutes 
                                      % INCREMENT maxfunevals successively to larger value(s)
            minfunevals = 'dim + 2';  % PUT MINIMAL SENSIBLE NUMBER OF EVALUATIONS for a restart
            maxrestarts = 1e4;        % SET to zero for an entirely deterministic algorithm

            more off;  % in octave pagination is on by default

            t0 = clock;
            rand('state', sum(100 * t0));

            %10: the number of runs of each algorithm
            dim = 10;  % small dimensions first, for CPU reasons
                  % 15 function instances
            for run = 1:nruns
                %disp(strcat(num2str(ifun),"_i", num2str(iinstance), " ", num2str(run), " ", num2str(currentAlgorithm)));
                fgeneric('initialize', ifun, iinstance, datapath, opt); 
                score = Algorithm{currentAlgorithm}(@callGecco, dim, 100,100);
                fgeneric('finalize');
                Data(run,currentAlgorithm) = score;
                %disp(Data);
          	end
        end
        [~,best] = min(mean(Data,1));
        best_i = 1;
        allBest(currentFunction,best_i) = best;
        %allBest(currentFunction,best_i) = mean(Data(:,best));
        %allBest(currentFunction,:) = mean(Data,1);
        save(strcat('E:\\matlab_results\\10\\results_', num2str(currentFunction), '.mat'), 'Data');
        disp(currentFunction);
        for a = [1:best-1,best+1:size(Data,2)]
          [~,diff] = ranksum(Data(:,a),Data(:,best));
          if ~diff
              best_i = best_i + 1;
              allBest(currentFunction,best_i) = a;
              %allBest(currentFunction,best_i) = mean(Data(:,a));
              disp("MULTIPLE BEST")
              disp([ifun iinstance])
              disp([a best])
              disp(Data)
              disp("------")
              disp(min(mean(Data)))
          end
        end
        
        
        currentFunction = currentFunction + 1;
        
    end
end
bestFinal = allBest;
save('E:\\matlab_results\\10\\all_best2.mat','allBest');
%disp(allBest);


