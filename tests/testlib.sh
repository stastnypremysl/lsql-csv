set -e 

tmp_folder=$(mktemp -d)
trap "rm -r $tmp_folder" EXIT


in_f=$tmp_folder/in_f
correct_out_f=$tmp_folder/correct_out_f
tmp_f=$tmp_folder/tmp_f

touch $in_f $correct_out_f


run_test(){
  if ./build/lsql-csv "$@" <$in_f | diff - $correct_out_f; then
    echo -e "Test \033[1m$(basename $0)\033[0m successed."
    exit 0
  else
    echo -e "Test \033[31;1m$(basename $0)\033[0m failed."
    exit 1
  fi
}
