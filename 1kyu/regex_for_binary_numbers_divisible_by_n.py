# https://www.codewars.com/kata/5993c1d917bc97d05d000068

import re


class BinaryNumberFSM():
    def __init__(self, number):
        self.num = number
        self.alphabet = {'0', '1'}
        self.states = set(map(str, range(number)))
        self.initial = '0'
        self.finals = {'0'}
        self.map = self.__create_transition_table()

    def __create_transition_table(self):
        map = {}
        for i in range(self.num):
            qx = ((i << 1)+0) % self.num
            qy = ((i << 1)+1) % self.num
            map[str(i)] = {str(qx): '0', str(qy): '1'}
        return map

    def print(self):
        print(self.alphabet)
        print(self.states)
        print(self.initial)
        print(self.finals)
        print(self.map)


def input_states(fsm, state):
    input_map = {}
    for kstate in fsm.map:
        for k in fsm.map[kstate]:
            if k == state:
                input_map[kstate] = fsm.map[kstate][k]
    return input_map


def create_regex_from_fsm(fsm):
    states = sorted(fsm.states)
    map = fsm.map
    while (len(map) > 1):
        current_state = states.pop()
        input_map = input_states(fsm, current_state)
        state_map = map.pop(current_state)

        for k1, v1 in input_map.items():
            for k2, v2 in state_map.items():
                if k1 != current_state and k2 != current_state:
                    if current_state in map[k1]:
                        map[k1].pop(current_state)
                    star = ''
                    if current_state in input_map:
                        star = input_map[current_state]
                        star = f"{star}*" if len(star) == 1 else f"({star})*"
                    if k2 in map[k1]:
                        map[k1][k2] = f"({map[k1][k2]}|{v1}{star}{v2})"
                    else:
                        map[k1][k2] = v1+star+v2
    return map['0']['0']+'+'


def regex_divisible_by(n):
    if n == 1:
        return "^(0|1)+$"
    machine = BinaryNumberFSM(n)
    expr = create_regex_from_fsm(machine)
    return f"^{expr}$"


# tests
def test(res, exp, message):
    if res and exp:
        print("ok")
    else:
        print(message)


if __name__ == "__main__":
    for i in range(1, 19):
        reg = re.compile(regex_divisible_by(i))
        for x in range(5, 13):
            test(bool(reg.search(bin(x)[2:])), x % i == 0,
                 f'Your result for n={i} was incorrect when matched against {x}')
