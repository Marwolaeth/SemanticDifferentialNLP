import sys
from sentence_transformers import SentenceTransformer, util
model = SentenceTransformer('paraphrase-MiniLM-L6-v2')
model = SentenceTransformer(
  'sentence-transformers/roberta-large-nli-stsb-mean-tokens'
)
sys.getsizeof(model)
model.bfloat16()
sys.getsizeof(model)

texts = r.texts
verbs = r.verbs

embeddings = model.encode(texts)
print(embeddings.shape)

similarities = util.pytorch_cos_sim(embeddings, embeddings)
print(similarities)

norms = model.encode(verbs)

similarities_norm = util.pytorch_cos_sim(embeddings, norms)
print(similarities_norm)
