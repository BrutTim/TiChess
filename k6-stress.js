import http from 'k6/http';
import { sleep, check } from 'k6';

export const options = {
  stages: [
    { duration: '10s', target: 200 }, // ramp up to 200
    { duration: '10s', target: 500 }, // ramp up to 500
    { duration: '10s', target: 1000 }, // ramp up to 1000
    { duration: '10s', target: 2000 }, // ramp up to 2000
    { duration: '10s', target: 5000 }, // ramp up to 5000
    { duration: '10s', target: 0 },    // scale down
  ],
};

export default function () {
  const res = http.get('http://localhost:8080/api/view/game');
  check(res, {
    'is status 200': (r) => r.status === 200,
  });
  // Very short sleep to hit the server hard
  sleep(0.1);
}
