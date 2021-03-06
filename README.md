# PooledMarketR

`PooledMarketR` is an R package built to provide the Pooled Market Dataset.

This dataset pools together the data from fopur similar prediction market on the results of systematic replication studies.

The systematic replication studies are:

- Replication Project: Psychology (Open Science Collaboration. _Estimating the reproducibility of psychological science._  Science 2015)
- Experimental Economics Replication Project (Camerer et al. _Evaluating replicability of laboratory experiments in economics._  Science 2016)
- Social Sciences Replication Project (Camerer et al. _Evaluating the Replicability of Social Science Experiments in Nature and Science._Nature Human Behaviour (2018))
- Many Labs 2 (Klein et al. _Many Labs 2: Investigating Variation in Replicability Across Samples and Settings_. Advances in Methods and Practices in Psychological Science 2018)

And the accompanying prediction markets are

- RPP - Dreber et al 2015 (PNAS)
- EERP - Camerer et al 2016 (Science)
- SSRP - Camerer et al 2018 (Nat Human Behav)
- ML2 - Forsell et al 2018

In the dataset the prediction markets are referred to as &#39;projects&#39;. I would recommend reading the above studies before using the package. The studies will explain the methodologies and provide context to the data.

Each project has 3 core datasets:

- **Outcome data**. This data set includes a binary variable where 0 indicates an unsuccessful replication   and 1 indicates successful replication. See individual studies for definitions of successful replication
- **Survey Data**. ** ** This includes individual responses to pre-market  surveys. See studies for specific survey information, such as question wording. The user\_id in this dataset are not unique across projects (i.e user 1 in one project is a different person than user 1 in another project). Also note that that responses are left unchanged. This relevant as participants belief of probability are sometimes recorded as integers (i.e 80%) and sometimes as fractions (0.8).
- **Market Data**. This includes individual transactions for each study. Final market prices are taken as market beliefs. Note that user\_id in this data relates to the same user\_id in the survey data.

 
