# !pip install transformers sentencepiece --quiet
import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification

# model_id = 'Marwolaeth/rubert-tiny-nli-terra-v0'
model_id = 'Marwolaeth/rosberta-nli-terra-v0'
tokenizer = AutoTokenizer.from_pretrained(model_id, revision='overfit')
model = AutoModelForSequenceClassification.from_pretrained(model_id, revision='overfit')
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
  model='Marwolaeth/rosberta-nli-terra-v0',
  revision='overfit'
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

nemkin_text = '''
Внимание, друзья!

Эксперты Kaspersky предупреждают о новых угрозах в мире кибербезопасности. За первые три месяца 2024 года число пользователей, ставших жертвами вредоносных приложений, маскирующихся под бесплатные VPN-сервисы, увеличилось в 2,5 раза!

Особенно под угрозой оказались юзеры, установившие такие приложения, как MaskVPN, ShieldVPN и PaladinVPN. Эти программы связаны с ботнетами и используются для кибератак и отмывания денег. В результате, более 19 миллионов IP-адресов в 190 странах мира уже оказались под контролем злоумышленников!

Но это еще не все! Бесплатные VPN-сервисы могут стать причиной утечки ваших персональных данных, включая сетевой трафик, пароли и банковские данные.

Будьте осторожны и выбирайте только проверенные и надежные VPN-сервисы, чтобы защитить свои данные и устройства!
'''

result = classifier(
  'classification: ' + nemkin_text,
  candidate_labels=['безопасно', 'опасно'],
  hypothesis_template='Пользоваться VPN {}'
)
pd.DataFrame(result)
# Raw:
# {'labels': ['опасно', 'безопасно'], 'scores': [0.7851947546005249, 0.2148052603006363]}
# {'labels': ['опасно', 'безопасно'], 'scores': [0.7849478721618652, 0.21505215764045715]}
# {'labels': ['опасно', 'безопасно'], 'scores': [0.7843124866485596, 0.21568752825260162]}
# Similarity+ :
# {'labels': ['опасно', 'безопасно'], 'scores': [0.9331448078155518, 0.06685522198677063]}
# {'labels': ['опасно', 'безопасно'], 'scores': [0.9332894086837769, 0.06671060621738434]}
# {'labels': ['опасно', 'безопасно'], 'scores': [0.9335148930549622, 0.06648512184619904]}
# Classification+ :
# {'labels': ['опасно', 'безопасно'], 'scores': [0.9332796335220337, 0.06672035157680511]}
# {'labels': ['опасно', 'безопасно'], 'scores': [0.9334080219268799, 0.06659197062253952]}
# {'labels': ['опасно', 'безопасно'], 'scores': [0.9335906505584717, 0.06640934199094772]}
