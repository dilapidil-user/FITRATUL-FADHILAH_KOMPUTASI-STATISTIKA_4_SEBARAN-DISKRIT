# 1. Jika rata-rata pelanggan datang ke toko ada;lah 3 orang per jam, modelkan 
#dengan Poisson dan hitung


# Rata-rata pelanggan datang = 3 orang per jam
lambda <- 3

# Model: X ~ Poisson(lambda = 3)

# Menghitung P(X >= 5)
P_X_ge_5 <- 1 - ppois(4, lambda)

cat("P(X >= 5) =", P_X_ge_5, "\n")
cat("P(X >= 5) =", round(P_X_ge_5 * 100, 2), "%\n")



# 2. Dari 100 bola (20 berwarna merah), diambil 10 tanpa pengaembalian. 
#Modelkan jumlah bola merah yang diambil dengan distribusi yang tepat
#Diketahui
# Total bola = 100
# Bola merah = 20
# Bola tidak merah = 80
# Bola yang diambil = 10
# Pengambilan TANPA pengembalian

N <- 100
K <- 20
n <- 10

# Model: X ~ Hypergeometric(N=100, K=20, n=10)

# Nilai X = 0 sampai 10
x_hyper <- 0:n

# Menghitung PMF
P_hyper <- dhyper(
  x_hyper,
  K,
  N - K,
  n
)

# Membuat tabel
tabel_hyper <- data.frame(
  Jumlah_Bola_Merah = x_hyper,
  Probabilitas = P_hyper
)

print(tabel_hyper)



# 3. Simulasikan 1000 percobaan binomial (n = 15, p = 0.4) dan 
#bandingkan histogram hasil simulasi dengan PMF teoretis


# X ~ Binomial(n=15, p=0.4)

set.seed(123)

# Simulasi 1.000 percobaan
hasil <- rbinom(
  n = 1000,
  size = 15,
  prob = 0.4
)

# Nilai X yang mungkin
x_binom <- 0:15

# PMF teoretis
pmf_binom <- dbinom(
  x_binom,
  size = 15,
  prob = 0.4
)

# Histogram hasil simulasi
hist(
  hasil,
  breaks = seq(-0.5, 15.5, by = 1),
  probability = TRUE,
  main = "Simulasi 1.000 Percobaan Binomial",
  xlab = "Jumlah Keberhasilan (X)",
  ylab = "Probabilitas"
)

# Menambahkan PMF teoretis
points(
  x_binom,
  pmf_binom,
  pch = 19
)

lines(
  x_binom,
  pmf_binom,
  type = "h",
  lwd = 2
)



# TABEL PMF TEORETIS BINOMIAL
# 
tabel_binom <- data.frame(
  X = x_binom,
  PMF_Teoretis = pmf_binom
)

print(tabel_binom)