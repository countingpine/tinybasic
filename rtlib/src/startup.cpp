int MAIN(int argc, char **argv);
int free_strings(int force);

int main(int argc, char **argv)
{
	int result;

	result = MAIN(argc, argv);

	free_strings(1);

	return result;
}
