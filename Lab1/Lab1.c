#include <stdio.h>
#include <stdlib.h>

// Функція для злиття двох підмасивів arr[low..mid] та arr[mid+1..high]
void merge(double arr[], int low, int mid, int high) {
    int i, j, k;
    int n1 = mid - low + 1;
    int n2 = high - mid;

    // Створення тимчасових масивів
    double left[n1], right[n2];

    // Копіювання даних у тимчасові масиви left[] та right[]
    for (i = 0; i < n1; i++)
        left[i] = arr[low + i];
    for (j = 0; j < n2; j++)
        right[j] = arr[mid + 1 + j];

    // Злиття тимчасових масивів у основний масив arr[]
    i = 0;
    j = 0;
    k = low;
    while (i < n1 && j < n2) {
        if (left[i] <= right[j]) {
            arr[k] = left[i];
            i++;
        } else {
            arr[k] = right[j];
            j++;
        }
        k++;
    }

    // Копіювання залишкових елементів left[], якщо такі є
    while (i < n1) {
        arr[k] = left[i];
        i++;
        k++;
    }

    // Копіювання залишкових елементів right[], якщо такі є
    while (j < n2) {
        arr[k] = right[j];
        j++;
        k++;
    }
}

// Функція для сортування чисельного масиву злиттям
void merge_sort(double arr[], int low, int high) {
    if (low < high) {
        // Знайдення середини масиву
        int mid = low + (high - low) / 2;

        // Рекурсивно сортуємо першу та другу половини
        merge_sort(arr, low, mid);
        merge_sort(arr, mid + 1, high);

        // Злиття сортованих підмасивів
        merge(arr, low, mid, high);
    }
}

// Функція для обчислення кумулятивної функції рівномірного розподілу
double uniform_cdf(double x, double min, double max) {
    return (x - min) / (max - min);
}

// Функція для визначення інтервалів та перетворення чисел у символи
void numerical_to_linguistic(double arr[], double sort_arr[], int size, char result[], int alphabet_size, char alphabet[]) {
    double min = sort_arr[0];
    double max = sort_arr[size - 1];
    double interval_size = 1.0 / alphabet_size; // Ймовірність для кожного інтервалу

    for (int i = 0; i < size; i++) {
        double cdf_value = uniform_cdf(arr[i], min, max);
        int interval_index = (int)(cdf_value / interval_size);
        if (interval_index >= alphabet_size) {
            interval_index = alphabet_size - 1;
        }
        result[i] = alphabet[interval_index];
    }
}

// Функція для побудови матриці передування
void build_precedence_matrix(char result[], int size, int alphabet_size, int matrix[alphabet_size][alphabet_size], char alphabet[]) {
    for (int i = 0; i < alphabet_size; i++) {
        for (int j = 0; j < alphabet_size; j++) {
            matrix[i][j] = 0;
        }
    }

    for (int i = 0; i < size - 1; i++) {
        int row = -1, col = -1;
        for (int j = 0; j < alphabet_size; j++) {
            if (result[i] == alphabet[j]) row = j;
            if (result[i+1] == alphabet[j]) col = j;
        }
        if (row != -1 && col != -1) {
            matrix[row][col]++;
        }
    }
}

int main() {
    // Відкриття файлу з параметрами
    FILE *params_file = fopen("params.txt", "r");
    if (params_file == NULL) {
        printf("Failed to open file with alphabet options.\n");
        return 1;
    }

    // Зчитування потужності алфавіту
    int alphabet_size;
    fscanf(params_file, "%d", &alphabet_size);

    // Зчитування алфавіту
    char alphabet[alphabet_size];
    for (int i = 0; i < alphabet_size; i++) {
        fscanf(params_file, " %c", &alphabet[i]);
    }
    fclose(params_file);

    // Відкриття вхідного файлу з числовим рядом
    FILE *input_file = fopen("number series.txt", "r");
    if (input_file == NULL) {
        printf("Could not open source file.\n");
        return 1;
    }

    // Зчитування чисел з файлу
    double numerical_series[5000];
    int size = 0;
    while (fscanf(input_file, "%lf", &numerical_series[size]) != EOF) {
        size++;
    }
    fclose(input_file);

    // Сортування чисельного ряду
    double sort_arr[size];
    for(int i=0; i<size; i++) {
        sort_arr[i] = numerical_series[i];
    }
    merge_sort(sort_arr, 0, size - 1);

    // Перетворення чисельного ряду у лінгвістичний ряд
    char result[size];
    numerical_to_linguistic(numerical_series, sort_arr, size, result, alphabet_size, alphabet);

    // Побудова матриці передування
    int matrix[alphabet_size][alphabet_size];
    build_precedence_matrix(result, size, alphabet_size, matrix, alphabet);

    // Виведення результатів у консоль
    printf("Linguistic series: \n");
    for (int i = 0; i < size; i++) {
        printf("%c ", result[i]);
    }
    printf("\n\n");

    printf("Precedence matrix:\n\t");
    for (int i = 0; i < alphabet_size; i++) {
        printf("%c\t", alphabet[i]);
    }
    printf("\n");
    for (int i = 0; i < alphabet_size; i++) {
        printf("%c\t", alphabet[i]);
        for (int j = 0; j < alphabet_size; j++) {
            printf("%d\t", matrix[i][j]);
        }
        printf("\n");
    }

    // Відкриття вихідного файлу
    FILE *output_file = fopen("output.txt", "w");
    if (output_file == NULL) {
        printf("Could not open source file.\n");
        return 1;
    }

    // Виведення результатів у файл
    fprintf(output_file, "Linguistic series: \n");
    for (int i = 0; i < size; i++) {
        fprintf(output_file, "%c ", result[i]);
    }
    fprintf(output_file, "\n\n");

    fprintf(output_file, "Precedence matrix:\n\t");
    for (int i = 0; i < alphabet_size; i++) {
        fprintf(output_file, "%c\t", alphabet[i]);
    }
    fprintf(output_file, "\n");
    for (int i = 0; i < alphabet_size; i++) {
        fprintf(output_file, "%c\t", alphabet[i]);
        for (int j = 0; j < alphabet_size; j++) {
            fprintf(output_file, "%d\t", matrix[i][j]);
        }
        fprintf(output_file, "\n");
    }

    fclose(output_file);
    printf("Data written to output.txt\n");

    return 0;
}
