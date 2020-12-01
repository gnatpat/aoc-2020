def main():
    seen = set()
    for line in open('input'):
        value = int(line)
        other = 2020 - value
        if other in seen:
            print(other * value)
            return
        seen.add(value)

def main3():
    seen = {}
    values = [int(v) for v in open('input')]
    for value in values:
        for value2 in values:
            seen[value + value2] = (value, value2)
    for value in values:
        other = 2020 - value
        if other in seen:
            value1, value2 = seen[other]
            print(value1, value2, value)
            print(value1 * value2 * value)
            return
        

if __name__=="__main__":
    main3()
