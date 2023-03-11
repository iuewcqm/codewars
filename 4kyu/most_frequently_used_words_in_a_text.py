# https://www.codewars.com/kata/51e056fe544cf36c410000fb

import re

def remove_separate_apostrophes(text):
    text = text.split()
    for word in text:
        if word.count("'") == len(word):
            text.remove(word)
    return ' '.join(text)

def get_treated_text(text):
    extra_chars = '!"#$%&()*+,-./:;<=>?@[\]^_`{|}~'
    regex = re.compile('[{}]'.format(re.escape(extra_chars)))
    text = remove_separate_apostrophes(regex.sub(' ', text))
    return text

def top_3_words(text):
    text = get_treated_text(text).lower().split()
    words_map = [(word, text.count(word)) for word in set(text)]
    words_map.sort(key=lambda x: x[1], reverse=True)
    return [word for word, _ in words_map[:3]]

if __name__ == "__main__":
    test_cases = [
        ("a a a  b  c c  d d d d  e e e e e", ["e", "d", "a"]),
        ("e e e e DDD ddd DdD: ddd ddd aa aA Aa, bb cc cC e e e", ["e", "ddd", "aa"]),
        ("  //wont won't won't ", ["won't", "wont"]),
        ("  , e   .. ", ["e"]),
        ("  ...  ", []),
        ("  '  ", []),
        ("  '''  ", []),
        ("a a c b b", ['a', 'b', 'c'])]
    for test, expected in test_cases:
        actual = top_3_words(test)
        if actual != expected:
            print(f"expected: {expected}, but was: {actual}")
