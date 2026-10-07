package algos

def modularIndex(m: Int, n: Int): Int = {
  return Math.floorMod((m - 1), n) + 1;
}

def killAtIndex(idx: Int, survivors: Array[Int]): Array[Int] = {
  val realIndex = modularIndex(idx, survivors.length);
  return survivors.take(realIndex - 1) ++ survivors.drop(realIndex);
}

def nextToEliminate(idx: Int, n: Int, k: Int): Int = {
  return modularIndex(idx + k - 1, n);
}

def naiveJosephusDynamics(
    survivors: Array[Int],
    eliminationIndex: Int,
    n: Int,
    k: Int
): Array[Int] = {
  if (n <= 1) return survivors;
  return naiveJosephusDynamics(
    killAtIndex(eliminationIndex, survivors),
    nextToEliminate(eliminationIndex, n - 1, k),
    n - 1,
    k
  );
}

def naiveJosephus(n: Int, k: Int): Int = {
  return naiveJosephusDynamics((1 to n).toArray, modularIndex(k, n), n, k)
    .apply(0);
}

def iterativeJosephus(n: Int, k: Int): Int = {
  return (2 to n).foldLeft(1)((prevJosephus, n) =>
    modularIndex(prevJosephus + k, n)
  )
}

def solutionMapping(x: Int, k: Int): Int = {
  return x + (x - 1) / (k - 1);
}

def reindexing(x: Int, n: Int, k: Int): Int = {
  return modularIndex(x - Math.floorMod(n, k), n - (n / k));
}

def josephus(n: Int, k: Int): Int = {
  if (n <= 1) {
    return 1;
  } else if (k == 1) {
    return n;
  } else if (n <= k) {
    return iterativeJosephus(n, k);
  } else {
    return solutionMapping(
      reindexing(josephus(n - n / k, k), n, k),
      k
    );
  }
}
