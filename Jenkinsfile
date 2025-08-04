pipeline {
    agent any

    stages {
        stage('Build Project Image') {
            steps {
                script {
                    echo 'Building the cl-freellock project Docker image...'
                    // Use sudo to run docker commands
                    sh 'sudo docker build -t cl-freelock-app:latest .'
                }
            }
        }
        stage('Run Tests') {
            steps {
                script {
                    echo 'Running tests inside the container...'
                    // Use sudo to run docker commands
                    sh 'sudo docker run --rm cl-freelock-app:latest'
                }
            }
        }
        stage('Run Benchmarks') {
            steps {
                script {
                    echo 'Running benchmarks inside the container...'
                    // Use sudo to run docker commands
                    sh 'sudo docker run --rm cl-freelock-app:latest make benchmark'
                }
            }
        }
    }
    post {
        always {
            echo 'Pipeline finished. Cleaning up build image...'
            // Use sudo to run docker commands
            sh 'sudo docker rmi cl-freelock-app:latest || true'
        }
    }
}
