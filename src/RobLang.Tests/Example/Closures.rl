fn make_adder(n):
  fn add_n(x):
    return x + n
  end
  return add_n(10)
end

print(make_adder(5))
print(make_adder(100))
