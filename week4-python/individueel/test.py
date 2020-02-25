import individual as ind

from os import listdir
from os.path import isfile, join
mypath = "../sudoku_boards/"

onlyfiles = [join(mypath, f) for f in listdir(mypath) if isfile(join(mypath, f))]



def main():
    print(ind.opgave1([1]))
    # print(ind.opgave1([1,2,3,5]))
    # print(ind.opgave1([4,3,2,1]))
    for i in ind.opgave2(["a",2,5,6,7]):
        print(i)
    # for i in ind.opgave2([10,1,5,6,7]):
    #
    #     print(i)


    # print(ind.opgave3a(onlyfiles[7]))
    # print(ind.opgave3a("sudoku.txt"))
    # ind.opgave3b(ind.opgave3a(onlyfiles[7]))

    # for file in onlyfiles:
    #     ind.opgave3(file)
    #     print()

    print(ind.sum_nested_it([[1], 2, 3, [[4], 5, [6]]]))
    print(ind.sum_nested_it([[ind.opgave2([10,1,5,6,7])], 2, 3, [4, 5, [6]]]))

if __name__ == "__main__":
    main()
