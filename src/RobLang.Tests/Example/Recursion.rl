fn factorial(n):
  if (n <= 1):
    return 1
  end
  return n * factorial(n - 1)
end

print(factorial(5))
print(factorial(1))
print(factorial(0))
