# https://www.codewars.com/kata/52d1bd3694d26f8d6e0000d3

class VigenereCipher(object):
    def __init__(self, key, alphabet):
        self.key = key
        self.alphabet = alphabet
        self.table = [self.alphabet[i:]+self.alphabet[:i] for i in range(len(self.alphabet))]

    def __encode_sym(self, p):
        if p[0] not in self.alphabet: return p[0]
        return self.table[ord(p[0])-ord(self.alphabet[0])][ord(p[1])-ord(self.alphabet[0])]

    def encode(self, text):
        pairs = zip(text, (self.key*len(text))[:len(text)])
        return "".join(map(self.__encode_sym, pairs))

    def __decode_sym(self, p):
        if p[0] not in self.alphabet: return p[0]
        return self.table[0][self.table[ord(p[1])-ord(self.alphabet[0])].index(p[0])]

    def decode(self, text):
        pairs = zip(text, (self.key*len(text))[:len(text)])
        return "".join(map(self.__decode_sym, pairs))
