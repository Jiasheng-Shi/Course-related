# 期中

依照统一的评分标注，期中没有可加分项。

---

## 第 1 题：4 分

本题共四小问。

**(a) 0 分。** 题目要求给出 \(\lambda_1/\lambda_2\) 的 MLE，但答卷中最后写成了两个参数估计量的比值形式，结果本身不可接受；同时 likelihood 的写法也不正确，尤其 indicator/support 的部分写错。

**(b) 0 分。** 本问要求证明 \(2m\lambda_1\bar X\sim \chi^2_{2m\alpha_1}\)。答卷中虽然写到 Gamma/Chi-square 的关系，但没有正确完成从 \(\sum X_i\) 到 \(2m\lambda_1\bar X\) 的变换推导，分布参数也处理不清，因此不能按评分细则给分。

**(c) 1 分。** 最终大致猜到了应当使用 \(F\)-distribution，并写出了接近的自由度形式；但中间过程基本缺失，尤其没有可靠地使用 (b) 中的 chi-square 结果和独立性来构造比例。因此只给结果方向上的 1 分。

**(d) 1 分。** 题目要求的是 \(\lambda_1/\lambda_2\) 的 one-sided confidence lower bound，但答卷写成了 two-sided confidence interval/test 的形式，方向和题目要求不一致。考虑到使用了前面 \(F\)-statistic 的想法，照顾性给 1 分。

此外，本题整体又酌情补了 2 分。因此第 1 题合计为

\[
0+0+1+1+2=4.
\]

---

## 第 2 题：2 分

本题的核心应是 hypergeometric likelihood：先写出

\[
P(k=10\mid N)=\frac{\binom{1000}{10}\binom{N-1000}{140}}{\binom{N}{150}},
\]

再用相邻比值 \(P(k=10\mid N+1)/P(k=10\mid N)\) 判断 likelihood 的增减，从而得到 MLE。

答卷中把问题近似写成 binomial distribution，这与题目中“不放回抽样”的设定不符，因此主要分布建模是错的；后续的 likelihood ratio 也不是评分细则中的正确 ratio 推导。

不过，答卷最后写出了 \(15000\) 这一与正确答案相邻/接近的结果，属于猜中一部分最终答案。故按最终结果对应的得分点象征性给 2 分。其余建模与推导分不能给。

---

## 第 3 题：12 分

本题当前给到 12 分。

(a) 中 population mean 的计算、用 sample mean 替代 population mean 的思路，以及 method of moments 的最终形式均可对应评分点，因此这一部分给满。

(b) 中前半部分可以给到 likelihood / log-likelihood 以及基本求导框架的步骤分，所以给到得分点 1 至 6。

从后面的 Fisher Information 具体计算开始，答卷中的 score function 与 second derivative 的具体形式均不正确，因此后续 Fisher Information 的计算结果不能继续给分。对应扣 3 分。

所以第 3 题为

\[
15-3=12.
\]

---

## 第 4 题：3 分

(a) 中写出了 \(\hat\theta=X_{(n)}=\max_i X_i\)，这是本题第一问的正确 MLE，因此给 3 分。

从 (b) 开始，答卷没有正确推出

\[
U=\frac{X_{(n)}}{\theta},\qquad F_U(u)=u^n,
\]

也没有正确处理 support。因此 (b) 的 CDF 推导分不能给。

(c) 的 exact confidence interval 依赖于 (b) 中正确的 pivotal distribution。由于前面的 pivotal distribution 已经错误，后续区间的 quantile 和不等式转换也不能成立。因此 (c) 不再给分。

所以第 4 题只保留第一问的 3 分。

---

## 第 5 题：22 分

本题主要扣分集中在后两处。

首先，(b) 中 asymptotic confidence interval 的形式有 scaling/parameterization 问题，因此扣 1 分。

其次，(d) 中应当基于 \(\hat\lambda=1/\bar Y=0.2\) 对 \(\lambda=0.25\) 作检验；正确结论应为不能拒绝 \(H_0\)。答卷中把区间写成了围绕 \(\bar Y=5\) 的形式，并据此得到 reject \(H_0\)，这是对参数 \(\lambda\) 与样本均值关系的误用，因此该处扣 2 分。

其余 MLE、Fisher information、test statistic/p-value 的主要结构可以对应评分细则给分。因此第 5 题为

\[
25-1-2=22.
\]

---

## 第 6 题：18 分

本题主要扣分在 (a)。答卷给出了两样本比例检验的大体形式，但 test statistic / p-value 的 denominator 与 pooled null variance 的处理不够规范，概率表达也不完全对应评分细则中的写法，因此扣 2 分。

(b) 的 power 推导主线可以接受，(c) 的 sample size 计算也得到正确结果，因此其余部分给分。

所以第 6 题为

\[
20-2=18.
\]

---

## 总分

\[
4+2+12+3+22+18=61.
\]
