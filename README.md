# thesis

This folder contains code to run topic modelling and generate different graphs on the output to analyse the result. The code is based on medical data combined into a corpus, i.e. a term document matrix and operations are performed in accordance to the demarcation of train and test data. The same can be applied to any data provided they are in the format of TDM.

The code for topic modelling uses: https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf

HDP uses: https://github.com/blei-lab/hdp

The folder structure is as follows:<br/>

1. k_eval_cv and k_eval_alpha_varying - evaluates for k topics and alpha using cross validation by meausring perplexity.<br/>
2. tm_{lda, ctm, hdp} - uses the lda, ctm and hdp techinques on a choosen set of k.<br/>
3. word_graph - creates a word cloud.<br/>
4. topic_dist_graph - creates a small multiple histogram of topic distribution across documents.<br/>
5. corr_graph - creates a correlation graph for the correlated topic modelling.<br/>
