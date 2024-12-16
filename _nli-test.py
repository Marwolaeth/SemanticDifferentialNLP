# !pip install transformers sentencepiece --quiet
import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification

# model_id = 'Marwolaeth/rubert-tiny-nli-terra-v0'
model_id = 'Marwolaeth/rosberta-nli-terra-v0'
tokenizer = AutoTokenizer.from_pretrained(model_id)
model = AutoModelForSequenceClassification.from_pretrained(model_id)
if torch.cuda.is_available():
    model.cuda()

# An example from the base model card
premise1 = 'Сократ - человек, а все люди смертны.'
hypothesis1 = 'Сократ никогда не умрёт.'
with torch.inference_mode():
    prediction = model(
      **tokenizer(premise1, hypothesis1, return_tensors='pt').to(model.device)
    )
    p = torch.softmax(prediction.logits, -1).cpu().numpy()[0]
print({v: p[k] for k, v in model.config.id2label.items()})
# {'not_entailment': 0.7698182, 'entailment': 0.23018183}
# {'not_entailment': 0.9382252, 'entailment': 0.061774753}

# An example concerning sentiments
premise2 = 'Я не люблю желтые занавески'
hypothesis2 = 'Мне нравятся желтые занавески'
with torch.inference_mode():
    prediction = model(
      **tokenizer(premise2, hypothesis2, return_tensors='pt').to(model.device)
    )
    p = torch.softmax(prediction.logits, -1).cpu().numpy()[0]
print({v: p[k] for k, v in model.config.id2label.items()})
# {'not_entailment': 0.60584205, 'entailment': 0.3941579}
# {'not_entailment': 0.9710297, 'entailment': 0.028970258}


# A tricky example
# Many NLI models fail to refute premise-hypothesis pairs like:
# 'It is good for our enemies that X' — 'It is good for us that X'
# This contradiction is quite clear, yet many NLI models struggle to accurately identify it, 
# highlighting their limitations in understanding conflicting sentiments in natural language inference.
premise3 = 'Для наших врагов хорошо, что это дерево красное.'
hypothesis3 = 'Для нас хорошо, что это дерево красное.'
with torch.inference_mode():
    prediction = model(
      **tokenizer(premise3, hypothesis3, return_tensors='pt').to(model.device)
    )
    p = torch.softmax(prediction.logits, -1).cpu().numpy()[0]
print({v: p[k] for k, v in model.config.id2label.items()})
# {'not_entailment': 0.54253, 'entailment': 0.45746994}
# {'not_entailment': 0.62343574, 'entailment': 0.3765643}

premise4 = 'в ркн, работают ебаные имбицылы, с мозгами как у курицы'
hypothesis4 = 'Я одобряю РКН'
# hypothesis4 = 'Я не одобряю РКН'
with torch.inference_mode():
    prediction = model(
      **tokenizer(premise4, hypothesis4, return_tensors='pt').to(model.device)
    )
    p = torch.softmax(prediction.logits, -1).cpu().numpy()[0]
print({v: p[k] for k, v in model.config.id2label.items()})
# {'not_entailment': 0.67889595, 'entailment': 0.32110408}
# {'not_entailment': 0.8785603, 'entailment': 0.121439725}


premise5 = 'I despise cats'
hypothesis5 = 'I love cats'
# hypothesis5 = 'Cats are bad'
with torch.inference_mode():
    prediction = model(
      **tokenizer(premise5, hypothesis5, return_tensors='pt').to(model.device)
    )
    p = torch.softmax(prediction.logits, -1).cpu().numpy()[0]
print({v: p[k] for k, v in model.config.id2label.items()})
# {'not_entailment': 0.9367107, 'entailment': 0.06328929}

from transformers import pipeline
import pandas as pd

classifier = pipeline(
  'zero-shot-classification',
  model='Marwolaeth/rosberta-nli-terra-v0'
)

classifier(
  'в ркн, работают ебаные имбицылы, с мозгами как у курицы',
  candidate_labels=['хороший', 'плохой'],
  hypothesis_template='РКН {}'
)

classifier(
  'similarity: в ркн, работают ебаные имбицылы, с мозгами как у курицы',
  candidate_labels=['хороший', 'плохой'],
  hypothesis_template='РКН {}'
)

result = classifier(
  [
    'similarity: I despise cats',
    'similarity: I love cats',
    'similarity: I hate cats',
    'similarity: I like cats'
  ],
  candidate_labels=['good', 'bad'],
  hypothesis_template='Cats are {}'
)
pd.DataFrame(result)
