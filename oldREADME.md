# Leveraging BERT-like Language Models for Semantic Differential Analysis: An Experimental Approach

This repository presents a study focused on using BERT-like language models for semantic differential analysis. The aim of the experiment is to evaluate the ability of different models to distinguish mentions of a brand as innovative or not innovative. We use clearly labelled data to ensure the reliability of the results.

## What is Semantic Differential?
The semantic differential is a measurement scale developed by psychologist Charles E. Osgood in the 1950s. It is designed to capture the connotative meaning of concepts by asking respondents to rate a given object (such as a brand) on a set of bipolar adjectives (e.g., "strong" vs. "weak"). This method allows researchers to quantify the emotional and evaluative dimensions of perceptions (Snider & Osgood, 1969).

Semantic differential scales are widely used in various fields, including psychology, marketing, and social sciences. This tool is particularly useful for understanding consumer perceptions and attitudes toward a brand in brand marketing. By analyzing how consumers view a brand on different semantic scales, marketers can identify strengths and weaknesses, tailor their messaging, and enhance brand positioning.

## Using Text Analysis to Approximate Semantic Differential Values
In the ever-evolving landscape of brand marketing, understanding consumer perceptions is crucial. Traditional methods of collecting semantic differential data often rely on surveys, which can be time-consuming and may not capture real-time sentiments. By leveraging text analysis, we can extract semantic differential values from large volumes of unstructured textual data—such as social media posts, reviews, or forums—providing a more dynamic and scalable approach to measuring consumer attitudes. This method allows marketers to gain insights into public sentiment and brand positioning without the constraints of conventional survey methodologies.

To approximate semantic differential values from textual data, we can employ various algorithms that utilize dictionaries and lexical resources. One notable approach involves using WordNet, a lexical database that groups English words into sets of synonyms (synsets) and provides relationships between them. By calculating semantic distances between these synsets, researchers can derive meaningful insights about how consumers perceive brands.

For instance, the paper "[Experiments with a Differential Semantics Annotation for WordNet 3.0](https://dl.acm.org/doi/pdf/10.5555/2107653.2107656)" (Tufiş & Ştefănescu, 2011) outlines a method where semantic distances are calculated to quantify perceptions. This approach can be integrated into our analysis pipeline by mapping keywords or phrases extracted from text to their corresponding synsets in WordNet. By analyzing the proximity of these synsets, we can approximate the semantic differential values associated with various attributes of a brand.

Modern natural language processing (NLP) methods address the challenges inherent in traditional rule-based approaches, particularly when calculating the semantic differential for entities like brands. In rule-based systems, creating a comprehensive list of words that enhance the meaning of an entity—such as verbs where the entity acts as the subject, adjectives that describe it, and adverbs that modify those verbs—can be labour-intensive and prone to errors. Additionally, it's important to consider negations and other modalities during this process. Ensuring that all relevant rules are considered often proves difficult, leading to incomplete analyses. However, advanced NLP models like BERT, along with others such as ELECTRA and RoBERTa, streamline this process by automatically identifying and contextualizing these words, significantly reducing the manual effort and improving accuracy in semantic evaluations.

## Outline of Proposed Approaches
In this section, we present three distinct approaches to enhance our understanding of the relative positions of target entities and bipolar words that define a semantic differential. We will experiment with two of these methods: Word Embedding Similarity and Natural Language Inference. While the Word Classification approach offers valuable insights, it requires additional resources and time for implementation, which we will not pursue at this stage. The following outline details each approach and its intended methodology.

### Word Classification
**Objective:** Train language models to classify words as either relevant or irrelevant to the meaning of a target word within a sentence. Once this classification is completed, we can employ various methods to approximate the semantic differential, using approaches such as ontology-based methods (like WordNet) or distributional semantics, including word embeddings.

**Methodology**:
- Utilize annotation tools like INCEpTION or LabelStudio to label training data.
- Define criteria for relevance based on semantic relationships.
**Note**: This approach will not be pursued due to the additional time required for tool implementation.


### Word Embedding Similarity
**Objective**: Determine the similarity of a target word's embedding, such as a brand name, with the two words from a bipolar pair (word norms) using advanced models like BERT, RoBERTa, DistilBERT, or ELECTRA.

**Methodology**:
- Generate embeddings for the target word and words in a bipolar pair (e.g., "strong" and "weak").
- Apply a similarity formula to determine the target word's position on the semantic scale.
- Analyze results to assess the relevance of the target word in context.


### Natural Language Inference
**Objective**: Leverage transformer models trained for Natural Language Inference (NLI) to perform zero-shot classification.

**Methodology**:
- Formulate hypotheses related to the target word (e.g., "X is weak" vs. "X is strong").
- Use NLI models to compare the probabilities of these hypotheses.
- Evaluate the outcome to derive insights about the semantic positioning of the target word.

--------
Special thanks to [Monica](https://monica.im/) for her assistance in data preparation and experiment planning. Throughout the project, we utilize the [text](https://www.r-text.org/) R package, which provides powerful tools for processing and analyzing texts using modern machine-learning methods (Kjell et al., 2023).

We invite you to explore the code and results, as well as contribute to the project!

## References
1. Snider, J.G., & Osgood, C.E. (1969). *Semantic Differential Technique: A Sourcebook*. Chicago: Aldine.
2. Kjell O, Giorgi S, Schwartz HA (2023). *The text-package: An R-package for Analyzing and Visualizing Human Language Using Natural Language Processing and Deep Learning.* Psychological Methods. doi:10.1037/met0000542, https://osf.io/preprints/psyarxiv/293kt/
3. Tufiş, D., & Ştefănescu, D. (2011). Experiments with a differential semantics annotation for WordNet 3.0. In *Proceedings of the 2nd Workshop on Computational Approaches to Subjectivity and Sentiment Analysis* (pp. 19–27). Association for Computational Linguistics. Portland, Oregon, USA.
