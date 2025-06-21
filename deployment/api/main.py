# Make sure to have IMAGE_NAME and EXAMPLES_DIR defined

import os
import shutil
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import docker
from docker.errors import APIError, ContainerError

app = FastAPI()
client = docker.from_env()

runner_image = os.getenv("IMAGE", default="ocamlrun")
examples_dir = os.getenv("EXAMPLES_DIR", default="../../examples")

class CodeRequest(BaseModel):
    code: str

class exampleRequest(BaseModel):
    file: str

@app.post("/run-code/")
def run_code(req: CodeRequest):
    temp_dir = os.path.join(os.getcwd(), "tmp/")
    try:
        os.makedirs(temp_dir, exist_ok=True)
        print(temp_dir)
        code_path = os.path.join(temp_dir, "main.ml")
        with open(code_path, "w") as f:
            f.write(req.code)

        print(f"Mounting {temp_dir}")
        container = client.containers.run(
            image=runner_image,
            volumes={temp_dir: {"bind": "/app", "mode": "rw"}},
            working_dir="/app",
            detach=True,
        )

        try:
            container.wait(timeout=10)
        except Exception:
            container.kill()
            raise HTTPException(status_code=408, detail="Execution timed out.")

        output_path = os.path.join(temp_dir, "output.txt")
        if os.path.exists(output_path):
            with open(output_path, "r") as f:
                output = f.read()
        else:
            output = "(output.txt not found)"

        return {"output": output}

    except (APIError, ContainerError) as e:
        raise HTTPException(status_code=400, detail=str(e))
    finally:
        # Cleanup
        shutil.rmtree(temp_dir)

@app.post("/list-examples/")
def list_examples():
    return os.listdir(examples_dir)

@app.post("/get-example/")
def get_example(request: exampleRequest):
    try:
        with open(os.path.join(examples_dir,request.file), 'r') as f:
            return {"output": f.read()}
    except FileNotFoundError as e:
        raise HTTPException(status_code=404, detail=f"{request.file} not found")