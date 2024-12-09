# !pip install transformers sentencepiece --quiet
import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification

model_id = 'Marwolaeth/rubert-tiny-nli-terra-v0'
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

# An example concerning sentiments
premise2 = 'Я ненавижу желтые занавески'
hypothesis2 = 'Мне нравятся желтые занавески'
with torch.inference_mode():
    prediction = model(
      **tokenizer(premise2, hypothesis2, return_tensors='pt').to(model.device)
    )
    p = torch.softmax(prediction.logits, -1).cpu().numpy()[0]
print({v: p[k] for k, v in model.config.id2label.items()})
# {'not_entailment': 0.60584205, 'entailment': 0.3941579}


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
