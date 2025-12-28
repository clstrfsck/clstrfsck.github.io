function equal_halves(s)
    n = length(s)
    n > 0 && iseven(n) && view(s, 1:n÷2) == view(s, n÷2+1:n)
end

function is_repeated_unit(s)
    length(s) > 1 && occursin(s, (s^2)[2:end-1])
end

function main()
    line = strip(read("y25d02.txt", String))

    nums = mapreduce(vcat, split(line, ',')) do part
        a, b = parse.(Int, split(part, '-'))
        a:b
    end

    result1 = sum(n for n in nums if equal_halves(string(n)))
    println("Result1: ", result1)

    result2 = sum(n for n in nums if is_repeated_unit(string(n)))
    println("Result2: ", result2)
end

main()