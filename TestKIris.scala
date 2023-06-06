object TestKIris {

  def main(args:Array[String]) : Unit =
    val ktest = KIris()
    // Recuperation de la matrice
    val mat = ktest.matrice()
    // nb cluster qui serait à définir manuellement
    val nombreCluster = 3
    // Creations des premiers centroids de facon aléatoire
    var Centroids = ktest.FirstCentroids(nombreCluster, mat)
    ktest.printMat(Centroids)
    // Tableau du nombre de Cluster
    val Clusters = new Array[Int](nombreCluster)
    // Nommage des Cluster
    for i<-0 until nombreCluster do
      Clusters(i)=i+1
    // Calcul les Premier Clusters  
    var NouveauCluster = ktest.AddToClusterAll(mat, Centroids)
    println(" ")
    ktest.ClusterNumber(NouveauCluster)
    // Variables de sauvegarde des anciens Clusters
    var AncienCluster : Array[Int] = Array.ofDim[Int](NouveauCluster.length)

    // Tant que les Cluster sont différents
    while !ktest.equals(AncienCluster, NouveauCluster) do
      // Recalcule des centroids
      Centroids = ktest.updateCentroids(mat, NouveauCluster, Clusters)
      AncienCluster = NouveauCluster
      // Attribution des valeurs aux clusters
      NouveauCluster = ktest.AddToClusterAll(mat,Centroids)
      println(" ")
      println("-----------------------------------------------------------")
      ktest.printTab(NouveauCluster)

    println("\n")
    println("---------------------------------------------")
    println("Le résultat est: ")
    ktest.printTab(NouveauCluster)
    println("\n ")
    ktest.ClusterNumber(NouveauCluster)
    println(" ")
    println("Les centroids finaux sont: ")
    ktest.printMat(Centroids)
    println("\n")
    println("Les Résultats apparaissant le plus souvent sont: \n Des Clusters de taille 150, 0, 0 \n Des Clusters de taille 50, 68, 32  ")
    println(" ")
}
