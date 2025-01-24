# Метод агрегирования

Данный параметр определяет метод агрегирования (англ. *aggregation* или *pooling*) для представления данных. Выберите один из следующих вариантов:

- **Автоматически**: 
  - Для моделей типа **sentence-transformers** агрегирование осуществляется по CLS-токену (в случае RoSBERTa вместо `[CLS]` используется `<s>`). Это позволяет получить общее представление для входного текста.
  - Для остальных моделей используется векторное представление имени объекта анализа.

- **CLS-токен**: 
  - Агрегирование производится исключительно по CLS-токену, что подходит для моделей, использующих этот подход.

- **Среднее по токенам**: 
  - Агрегирование осуществляется путем вычисления среднего векторного представления всех токенов, что является опцией по умолчанию в библиотеке `text`.

- **Токен объекта**: 
  - Используется векторное представление конкретного токена объекта анализа, что позволяет акцентировать внимание на семантических нюансах, которые векторное представление объекта усвоило через контекстное кодирование.