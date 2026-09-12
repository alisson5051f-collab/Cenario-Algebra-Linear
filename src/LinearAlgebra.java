public class LinearAlgebra {
//Alisson
     public static Matrix transpose(Matrix a) {
        int rows = a.getRows();
        int cols = a.getCols();
        double[][] res = new double[cols][rows];

        for (int i = 1; i <= rows; i++) {
            for (int j = 1; j <= cols; j++) {
                res[j - 1][i - 1] = a.get(i, j);
            }
        }
        return new Matrix(cols, rows, res);
    }

     
        public static Matrix transpose(Vector a) {
        int dim = a.getDim();
        double[][] res = new double[1][dim];

        for (int i = 1; i <= dim; i++) {
            res[0][i - 1] = a.get(i);
        }
        return new Matrix(1, dim, res);
    }


        public static Matrix sum(Matrix a, Matrix b) {
        if (a.getRows() != b.getRows() || a.getCols() != b.getCols()) {
            System.out.println("Erro: As matrizes devem ter as mesmas dimensões.");
            return null;
        }

        int rows = a.getRows();
        int cols = a.getCols();
        double[][] res = new double[rows][cols];

        for (int i = 1; i <= rows; i++) {
            for (int j = 1; j <= cols; j++) {
                res[i - 1][j - 1] = a.get(i, j) + b.get(i, j);
            }
        }
        return new Matrix(rows, cols, res);
    }

        public static Vector sum(Vector a, Vector b) {
        if (a.getDim() != b.getDim()) {
            System.out.println("Erro: Os vetores devem ter a mesma dimensão.");
            return null;
        }

        int dim = a.getDim();
        double[] res = new double[dim];

        for (int i = 1; i <= dim; i++) {
            res[i - 1] = a.get(i) + b.get(i);
        }
        return new Vector(res, dim);
    }



        public static Matrix times(Matrix a, Matrix b) {
        if (a.getRows() != b.getRows() || a.getCols() != b.getCols()) {
            System.out.println("Erro: As matrizes devem ter as mesmas dimensões.");
            return null;
        }

        int rows = a.getRows();
        int cols = a.getCols();
        double[][] res = new double[rows][cols];

        for (int i = 1; i <= rows; i++) {
            for (int j = 1; j <= cols; j++) {
                res[i - 1][j - 1] = a.get(i, j) * b.get(i, j);
            }
        }
        return new Matrix(rows, cols, res);
    }



      public static Vector times(Vector a, Vector b) {
        if (a.getDim() != b.getDim()) {
            System.out.println("Erro: Os vetores devem ter a mesma dimensão.");
            return null;
        }

        int dim = a.getDim();
        double[] res = new double[dim];

        for (int i = 1; i <= dim; i++) {
            res[i - 1] = a.get(i) * b.get(i);
        }
        return new Vector(res, dim);
    }



        public static Matrix times(double a, Matrix b) {
        int rows = b.getRows();
        int cols = b.getCols();
        double[][] res = new double[rows][cols];

        for (int i = 1; i <= rows; i++) {
            for (int j = 1; j <= cols; j++) {
                res[i - 1][j - 1] = a * b.get(i, j);
            }
        }
        return new Matrix(rows, cols, res);
    }



       public static Matrix times(Matrix a, double b) {
        return times(b, a);
    }



        public static Vector times(double a, Vector b) {
        int dim = b.getDim();
        double[] res = new double[dim];

        for (int i = 1; i <= dim; i++) {
            res[i - 1] = a * b.get(i);
        }
        return new Vector(res, dim);
    }



        public static Vector times(Vector a, double b) {
        return times(b, a);
    }
}

     //parte do Enzo
     public static Matrix dot(Matrix a, Matrix b) 
        if (a.getCols() != b.getRows()) {
            System.out.println("Erro: A quantidade de colunas da primeira matriz deve ser igual à quantidade de linhas da segunda matriz.");
            return null;
        }

        int rows = a.getRows();
        int cols = b.getCols();
        

        double[][] resultElements = new double[rows][cols];

        for (int i = 1; i <= rows; i++) {
            for (int j = 1; j <= cols; j++) {
                double sum = 0;
                for (int k = 1; k <= a.getCols(); k++) {
                  
                    sum += a.get(i, k) * b.get(k, j);
                }
 
                resultElements[i-1][j-1] = sum; 
            }
        }

        return new Matrix(rows, cols, resultElements);
    }

    public static Matrix gauss(Matrix a) {
        int rows = a.getRows();
        int cols = a.getCols();

        double[][] copyElements = new double[rows][cols];
        for (int i = 1; i <= rows; i++) {
            for (int j = 1; j <= cols; j++) {
                copyElements[i-1][j-1] = a.get(i, j);
            }
        }
        Matrix res = new Matrix(rows, cols, copyElements);

        // Processo de eliminação
        for (int k = 1; k <= Math.min(rows, cols); k++) {
            
            int maxRow = k;
            double maxVal = Math.abs(res.get(k, k));
            for (int i = k + 1; i <= rows; i++) {
                if (Math.abs(res.get(i, k)) > maxVal) {
                    maxVal = Math.abs(res.get(i, k));
                    maxRow = i;
                }
            }

            if (maxRow != k) {
                for (int j = 1; j <= cols; j++) {
                    double temp = res.get(k, j);
                    res.set(k, j, res.get(maxRow, j));
                    res.set(maxRow, j, temp);
                }
            }

            if (Math.abs(res.get(k, k)) < 1e-10) {
                continue;
            }

            for (int i = k + 1; i <= rows; i++) {
                double factor = res.get(i, k) / res.get(k, k);
                res.set(i, k, 0.0); // Zera explicitamente
                for (int j = k + 1; j <= cols; j++) {
                    double newValue = res.get(i, j) - factor * res.get(k, j);
                    res.set(i, j, newValue);
                }
            }
        }

        return res;
    }

    public static Matrix solve(Matrix a) {
        Matrix echelon = gauss(a);
        int rows = echelon.getRows();
        int cols = echelon.getCols();

        if (cols <= rows) {
            System.out.println("Erro: A matriz não representa um sistema linear resolvível (precisa ser aumentada).");
            return null;
        }
        double[][] solElements = new double[rows][1];


        for (int i = rows; i >= 1; i--) {
            double sum = echelon.get(i, cols); 
            
            for (int j = i + 1; j <= rows; j++) {
                sum -= echelon.get(i, j) * solElements[j-1][0];
            }
            
            double pivot = echelon.get(i, i);
            if (Math.abs(pivot) < 1e-10) {
                System.out.println("Aviso: O sistema não tem solução única (indeterminado ou sem solução).");
                return null;
            }

            solElements[i-1][0] = sum / pivot;
        }

        return new Matrix(rows, 1, solElements);
    }
    public static void main(String[] args) {
        //os testes aq
 System.out.println("--- Teste do Método: dot (Multiplicação) ---");
        double[][] valA = {{1, 2}, {3, 4}};
        double[][] valB = {{2, 0}, {1, 2}};
        Matrix mA = new Matrix(2, 2, valA);
        Matrix mB = new Matrix(2, 2, valB);
        Matrix mDot = dot(mA, mB);
        if (mDot != null) mDot.printMatrix();

        System.out.println("\n--- Teste do Método: gauss (Eliminação) ---");
        double[][] valG = {
            {2, 1, -1, 8}, 
            {-3, -1, 2, -11}, 
            {-2, 1, 2, -3}
        };
        Matrix mG = new Matrix(3, 4, valG);
        Matrix mGauss = gauss(mG);
        mGauss.printMatrix();

        System.out.println("\n--Teste do Método: solve (Resolução Sistema Linear) ---");
        Matrix solution = solve(mG);
        if (solution != null) solution.printMatrix();
    }
}
