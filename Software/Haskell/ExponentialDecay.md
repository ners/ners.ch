# Exponential decay

We wish to simulate [exponential decay] on a set of discrete elements over time.

We run our simulation at a frequency of $\frac{1\ \text{s}}{\delta t}\ \text{Hz}$. Each simulation step happens $\delta t$ after the previous.

Our simulation speed relates to the half-life $t_{1/2}$ with the coefficient $k =\frac{t_{1/2}}{\delta t}$.

On average, we expect to decay half of the remaining elements every $t_{1/2}$, or on every $k$ simulation steps.

On each simulation step we expect to decay $N_t (1 - 2^{-1/k})$, where $N_t$ is the number of remaining elements.

Proof:

$$
\begin{split}
decay(N) & = N - N (1 - 2^{-1/k}) \\
         & = N\ 2^{-1/k} \\
decay^{\circ k}(N) & = N\ 2^{-k/k} = \frac{N}{2}\ \square
\end{split}
$$

At each simulation step, each element has a probability $p = 2^{-1/k}$ to survive the step, and a probability $q = 1-p$ to decay.

Finally, on each simulation step, we decay $\delta N$ of the remaining elements. $\delta N$ is a [binomially distributed][binomial distribution] random variable:

$$
\begin{split}
\delta N & \sim B(N_t, q)
\\
E[\delta N] & = N_t\ q = N_t (1 - 2^{-1/k})
\end{split}
$$

[exponential decay]: https://en.wikipedia.org/wiki/Exponential_decay
[binomial distribution]: https://en.wikipedia.org/wiki/Binomial_distribution
