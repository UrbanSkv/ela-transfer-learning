%Calculates the friedman values of the problem distributions.

function fried = calculate_friedman()
    fried = [];
    for sample_size_1 = [500 1000 5000 10000]
        fried_1 = [];
        for sample_size_2 = [500 1000 5000 10000]
            disp([sample_size_1 sample_size_2]);
            results_matrix = transform_all(sample_size_1);
            if (sample_size_1 ~= sample_size_2)
                results_matrix = [results_matrix transform_all(sample_size_2)];
            end
            f_results = friedman(results_matrix);
            fried_1 = [fried_1 f_results];
        end
        fried = [fried; fried_1];
    end
end