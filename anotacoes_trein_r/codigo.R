

dados <- c(1,2,2,3,3,4,4)

soma <-0
for (i in 1:length(dados)) {
  soma = soma+dados[i]
}
print(soma)


sum(dados)