import torch
from torch import Tensor
import torch.nn.functional as F
from transformers import AutoTokenizer, AutoModel


def index(tensor: Tensor, value, ith_match:int = 0) -> Tensor:
    """
    Returns generalized index (i.e. location/coordinate) of the first occurence of value
    in Tensor. For flat tensors (i.e. arrays/lists) it returns the indices of the occurrences
    of the value you are looking for. Otherwise, it returns the "index" as a coordinate.
    If there are multiple occurences then you need to choose which one you want with ith_index.
    e.g. ith_index=0 gives first occurence.

    Reference: https://stackoverflow.com/a/67175757/1601580
    :return:
    """
    # bool tensor of where value occurred
    places_where_value_occurs = (tensor == value)
    # get matches as a "coordinate list" where occurence happened
    matches = (tensor == value).nonzero()  # [number_of_matches, tensor_dimension]
    if matches.size(0) == 0:  # no matches
        return 0
    else:
        # get index/coordinate of the occurence you want (e.g. 1st occurence ith_match=0)
        index = matches[ith_match]
        return index

inputs = [
  # 
  "classification: Он нам и <unk> не нужон ваш Интернет!",
  "classification: Компания <obj> — самая инновационная.",
  # 
  "classification: What a time to be alive!",
  "classification: Инновационный, продвинутый, изобретательный"
]

tokenizer = AutoTokenizer.from_pretrained("ai-forever/ru-en-RoSBERTa")
model = AutoModel.from_pretrained("ai-forever/ru-en-RoSBERTa")

tokenized_inputs = tokenizer(inputs, max_length=512, padding=True, truncation=True, return_tensors="pt")
tokenized_input = tokenizer(inputs[1], max_length=512, padding=True, truncation=True, return_tensors="pt")

tokenizer.add_tokens(["<obj>"])
tokenizer.convert_ids_to_tokens(98505)

token_ids = tokenized_inputs['input_ids']
object_positions = [index(doc, 98505) for doc in token_ids]
tokens = [emb[doc, ids] for doc, ids in zip(range(len(token_ids)), object_positions)]

def pool(hidden_state, mask, token_ids, pooling_method="cls"):
    if pooling_method == "mean":
        s = torch.sum(hidden_state * mask.unsqueeze(-1).float(), dim=1)
        d = mask.sum(axis=1, keepdim=True).float()
        return s / d
    elif pooling_method == "cls":
        return hidden_state[:, 0]
    elif pooling_method == "obj":
        object_positions = [index(doc, 98505) for doc in token_ids]
        tokens = 


with torch.no_grad():
    outputs = model(**tokenized_inputs)
    
embeddings = pool(
    outputs.last_hidden_state, 
    tokenized_inputs["attention_mask"],
    pooling_method="cls" # or try "mean"
)

embeddings = F.normalize(embeddings, p=2, dim=1)

sim_scores = embeddings[:3] @ embeddings[3:].T
print(sim_scores.diag().tolist())
# [0.4796873927116394, 0.9409002065658569, 0.7761015892028809]
