pipeline {
    agent any

    stages {
        stage('Build Project Image') {
            steps {
                script {
                    echo 'Building the cl-freelock project Docker image...'
                    sh 'docker build -t cl-freelock-app:latest .'
                }
            }
        }
        stage('Run Tests') {
            steps {
                script {
                    echo 'Running tests inside the container...'
                    // Runs the default CMD ["make", "test"] from the Dockerfile
                    sh 'docker run --rm cl-freelock-app:latest'
                }
            }
        }
        stage('Run Benchmarks') {
            steps {
                script {
                    echo 'Running benchmarks inside the container...'
                    // To run a non-default command, we specify it at the end
                    sh 'docker run --rm cl-freelock-app:latest make benchmark'
                }
            }
        }
    }
    post {
        always {
            echo 'Pipeline finished. Cleaning up build image...'
            sh 'docker rmi cl-freelock-app:latest || true'
        }
    }
}
