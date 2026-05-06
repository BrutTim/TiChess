import http from 'k6/http';
import { sleep, check } from 'k6';

export const options = {
  stages: [
    { duration: '10s', target: 50 }, // ramp up
    { duration: '20s', target: 50 }, // stable
    { duration: '10s', target: 0 },  // ramp down
  ],
  thresholds: {
    http_req_duration: ['p(95)<500'], // p95 should be < 500ms
    http_req_failed: ['rate<0.01'],   // error rate < 1%
  },
};

export default function () {
  const res = http.get('http://localhost:8080/api/view/game');
  check(res, {
    'is status 200': (r) => r.status === 200,
  });
  sleep(1);
}
