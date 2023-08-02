import subprocess
import lldb

def setup_swipl_path():
    swipl_path = subprocess.check_output("cargo swipl info -l", shell=True).decode('UTF-8').strip()
    old_path = lldb.target.GetEnvironment().Get('LD_LIBRARY_PATH')
    if old_path:
        new_path = f"{swipl_path}:{old_path}"
    else:
        new_path = swipl_path
    lldb.debugger.HandleCommand(f'env LD_LIBRARY_PATH="{new_path}"')
