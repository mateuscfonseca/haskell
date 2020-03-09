def head(arr): return arr[0] if len(arr) > 0 else None
def tail(arr): return arr[1:] if len(arr) > 0 else []

def doubleEveryOther(arr):
    if (len(arr) == 0): return []
    return [head(arr) * 2] + ([arr[1]] if len(arr) > 1 else []) + doubleEveryOther(arr[2:])

print(doubleEveryOther([1,2,3,4,5,6,7]))
