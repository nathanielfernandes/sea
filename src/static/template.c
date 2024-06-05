/* {{lib}} */

/* {{function definitions}} */

/* {{functions}} */

Result __main() {
    /* {{main}} */
}

int main() {
    Result res = __main();
    if (res.type == ERR) {
        printf("Error: ");
        println(res.data.Err);
        // return 1;
    }

    return 0;
}