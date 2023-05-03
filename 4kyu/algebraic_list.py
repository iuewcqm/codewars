# https://www.codewars.com/kata/529a92d9aba78c356b000353

class Cons:
    def __init__(self, head, tail):
        self.head = head
        self.tail = tail
      
    def to_array(self):
        tail = (self.tail.to_array() if self.tail is not None else [])
        return [self.head] + tail
    
    @classmethod
    def from_array(cls, arr):
        if arr:
            return Cons(arr[0], Cons.from_array(arr[1:]))

    def filter(self, fn):
        tail = self.tail and self.tail.filter(fn)
        return Cons(self.head, tail) if fn(self.head) else tail
    
    def map(self, fn):
        tail = self.tail and self.tail.map(fn)
        return Cons(fn(self.head), tail)

if __name__ == '__main__':
    print(Cons.from_array([1,2,3,4,5])
                .filter(lambda n: n>3)
                .to_array())
    print(Cons.from_array(["1","2","3","4","5"])
                .map(int)
                .filter(lambda n: n%2==0)
                .to_array())
