# https://www.codewars.com/kata/55983863da40caa2c900004e

def next_bigger(n: int) -> int:
    s_id = find_split_index(str(n))
    if s_id == -1: return -1
    s    = swap_next_bigger_digits(str(n), s_id)
    s    = int(sort_last_part(s, s_id))
    return s if s > n else -1

def find_split_index(n: str) -> int:
    for i in range(len(n)-1, 0, -1):
        if n[i] > n[i-1]:
            return i-1
    return -1

def swap_next_bigger_digits(s: str, s_id: int) -> str:
    s = list(s)
    part = s[s_id:]
    n_id = part.index(min([i for i in part if i>part[0]]))+s_id
    s[s_id], s[n_id] = s[n_id], s[s_id]
    return "".join(s)

def sort_last_part(s: str, s_id: int) -> str:
    return s[:s_id+1] + "".join(sorted(s[s_id+1:]))

        
if __name__ == '__main__':
    print(next_bigger(59884848459853))
